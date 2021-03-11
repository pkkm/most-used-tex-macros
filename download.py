#!/usr/bin/env python3
# pylint: disable=too-few-public-methods

"""Download top GitHub repositories (by stars) in a given language."""

import os
import subprocess
import sys
import dataclasses
import argparse
import time
import datetime
import github
import requests

def message(text, overwrite_prev_line=False, file_=sys.stderr):
    """Show a progress message."""
    if overwrite_prev_line and file_.isatty():
        file_.write("\x1b[1F\x1b[2K" + text + "\n")
        # Escape sequences: go to first character of previous line, erase line.
    else:
        print(text, file=file_)

def download_file(
        method, url, file_, progress_callback=lambda: None, **kwargs):
    """Download a file without loading the entire response into memory.
    Example: `download_file("GET", "https://google.com", "google.html")`
    """

    response = requests.request(method, url, stream=True, **kwargs)
    response.raise_for_status() # Throw an error for bad status codes.

    bytes_total = response.headers.get("content-length")
    if bytes_total is not None:
        bytes_total = int(bytes_total)
    bytes_downloaded = 0

    with open(file_, "wb") as handle:
        for block in response.iter_content(32 * 1024):
            handle.write(block)
            bytes_downloaded += len(block)
            progress_callback(bytes_downloaded, bytes_total)

def extract_tar(archive, directory, strip_components=0):
    """Extract `archive` into `directory`."""
    args = ["tar", "xf", archive, "--directory={}".format(directory)]
    if strip_components > 0:
        args.append("--strip-components={}".format(strip_components))
    subprocess.run(args, check=True)
    # An alternative would be to use the `tarfile` module, but it has worse
    # security (it may extract outside the given directory).

@dataclasses.dataclass
class Repo:
    """Information about a GitHub repository to download."""
    name: str
    tarball_url: str
    archive_path: str
    extract_path: str

def get_repo(repo, remove_after_extraction=False, progress_str=None):
    """Download and extract the `Repo` object `repo`."""

    if progress_str is not None:
        progress_formatted = " ({})".format(progress_str)
    else:
        progress_formatted = ""

    if os.path.isdir(repo.extract_path):
        message("Skipping already extracted{}: {}".format(
            progress_formatted, repo.name))
        return

    message("Downloading{}: {}".format(progress_formatted, repo.name))

    def download_progress(bytes_downloaded, bytes_total):
        download_progress_str = ""
        if bytes_total is not None and bytes_total > 0:
            download_progress_str += "{:.1%}, ".format(
                bytes_downloaded / bytes_total)
        download_progress_str += "{:.1f} MiB".format(
            bytes_downloaded / (1024 * 1024))

        message(
            "Downloading{}: {} ({})".format(
                progress_formatted, repo.name, download_progress_str),
            overwrite_prev_line=True)

    download_file("GET", repo.tarball_url, repo.archive_path, download_progress)

    message("Extracting{}: {}".format(progress_formatted, repo.name))

    os.mkdir(repo.extract_path)
    extract_tar(repo.archive_path, repo.extract_path, strip_components=1)

    if remove_after_extraction:
        os.remove(repo.archive_path)

def parse_args():
    """Parse command-line arguments."""

    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument(
        "--n-repos", type=int, default=1000,
        help="how many repositories to download")

    parser.add_argument(
        "--token",
        help="GitHub token (recommended because authenticated users have " +
        "higher rate limits)")

    parser.add_argument(
        "--language", default="TeX",
        help="download repositories in this language")

    return parser.parse_args()

def get_repo_search_results(ghub, n_repos, *args, **kwargs):
    """Get `n_repos` results from the search described by `args` and `kwargs`
    (which will be passed to `search_repositories`). Retrieve all the results
    instead of lazy-loading them to minimize the chance of the results changing
    while we're searching.
    """

    message("Getting search results")

    search_results = ghub.search_repositories(*args, **kwargs)

    result = []

    for i_repo, repo in enumerate(search_results[:n_repos]):
        # Respect the rate limit for search. Workaround for
        # <https://github.com/PyGithub/PyGithub/issues/1319> (the github module
        # uses the global rate limit while ignoring the search-specific one).
        if (i_repo + 1) % 30 == 0:
            limit = ghub.get_rate_limit() # This doesn't count towards the limit.
            if limit.search.remaining == 0:
                now = datetime.datetime.now(datetime.timezone.utc)
                reset = limit.search.reset.replace(tzinfo=datetime.timezone.utc)
                seconds_to_sleep = (reset - now).total_seconds() + 5
        else:
            seconds_to_sleep = 0

        message(
            "Getting search results ({}/{}{})".format(
                i_repo + 1,
                n_repos,
                ", waiting due to rate limit" if seconds_to_sleep > 0 else ""),
            overwrite_prev_line=True)

        if seconds_to_sleep > 0:
            time.sleep(seconds_to_sleep)

        result.append(repo)

    return result

def get_repo_data(repos):
    """Convert `github.Repository.Repository` objects into `Repo` by retrieving
    appropriate information.
    """

    message("Getting repository data")

    result = []

    for i_repo, repo in enumerate(repos):
        message(
            "Getting repository data ({}/{})".format(
                i_repo + 1, len(repos)),
            overwrite_prev_line=True)

        result.append(Repo(
            name=repo.full_name,
            tarball_url=repo.get_archive_link("tarball"),
            archive_path=os.path.join(
                "repos", "archives", "{} {}.tar.gz".format(
                    repo.owner.login, repo.name)),
            extract_path=os.path.join(
                "repos", "extracted", "{} {}".format(
                    repo.owner.login, repo.name))))

    return result

def main():
    """Main entry point."""

    args = parse_args()

    ghub = github.Github(args.token)

    repos = get_repo_search_results(
        ghub,
        args.n_repos,
        "stars:>0 language:{}".format(args.language),
        sort="stars",
        order="desc")

    to_download = get_repo_data(repos)

    os.makedirs(os.path.join("repos", "archives"), exist_ok=True)
    os.makedirs(os.path.join("repos", "extracted"), exist_ok=True)

    for i_repo, repo_info in enumerate(to_download):
        get_repo(
            repo_info,
            progress_str="{}/{}".format(i_repo + 1, len(to_download)))

if __name__ == "__main__":
    main()
