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
import shutil
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

    with requests.request(method, url, stream=True, **kwargs) as response:
        response.raise_for_status() # Raise an error for bad status codes.

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
    clone_url: str
    archive_path: str
    extract_path: str

def download_repo_by_archive(
        repo, remove_after_extraction=True, progress_str=None):
    """Download and extract the archive for the `Repo` object `repo`."""

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

def download_repo_by_clone(repo, remove_dotgit=True, progress_str=None):
    """Clone the `Repo` object `repo`."""

    if progress_str is not None:
        progress_formatted = " ({})".format(progress_str)
    else:
        progress_formatted = ""

    if os.path.isdir(repo.extract_path):
        message("Skipping already cloned{}: {}".format(
            progress_formatted, repo.name))
        return

    message("Cloning{}: {}".format(progress_formatted, repo.name))

    subprocess.run(
        ["git", "-c", "http.postBuffer=1G", "clone", "--quiet", "--depth=1",
         "--", repo.clone_url, repo.extract_path],
        check=True)

    if remove_dotgit:
        shutil.rmtree(os.path.join(repo.extract_path, ".git"))

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
        progress_str = "Getting search results ({}/{})".format(
            i_repo + 1, n_repos)
        message(progress_str, overwrite_prev_line=True)

        # Respect the rate limit for search. Workaround for
        # <https://github.com/PyGithub/PyGithub/issues/1319> (the github module
        # uses the global rate limit while ignoring the search-specific one).
        if (i_repo + 1) % 30 == 0:
            limit = ghub.get_rate_limit() # This doesn't count towards the limit.
            if limit.search.remaining == 0:
                now = datetime.datetime.now(datetime.timezone.utc)
                reset = limit.search.reset.replace(tzinfo=datetime.timezone.utc)
                seconds_to_sleep = (reset - now).total_seconds() + 5
                if seconds_to_sleep > 0:
                    message(
                        progress_str + " (waiting due to the rate limit)",
                        overwrite_prev_line=True)
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
            clone_url=repo.clone_url,
            archive_path=os.path.join(
                "repos", "archives", "{} {}.tar.gz".format(
                    repo.owner.login, repo.name)),
            extract_path=os.path.join(
                "repos", "ready", "{} {}".format(
                    repo.owner.login, repo.name))))

    return result

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

    parser.add_argument(
        "--method", choices=["archive", "clone"], default="clone",
        help="whether to download and extract .tar.gz archives from GitHub " +
        "or clone the repos (more reliable)")
    # Getting the repos by cloning is more reliable than by downloading archives
    # because as of 2021-03, GitHub drops connections after about 20 minutes of
    # downloading, causing an `IncompleteRead(0 bytes read)` error. The problem
    # isn't with this program because this also happens with other download
    # utilities -- for example, wget shows `Read error at byte <number>`.

    parser.add_argument(
        "--keep-archives", action="store_true",
        help="keep archives after extraction (when --method is `archive`)")

    parser.add_argument(
        "--keep-dotgit", action="store_true",
        help="keep .git after cloning (when --method is `clone`)")

    return parser.parse_args()

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

    if args.method == "archive":
        os.makedirs(os.path.join("repos", "archives"), exist_ok=True)
    os.makedirs(os.path.join("repos", "ready"), exist_ok=True)

    for i_repo, repo_info in enumerate(to_download):
        progress_str = "{}/{}".format(i_repo + 1, len(to_download))
        if args.method == "archive":
            download_repo_by_archive(
                repo_info,
                remove_after_extraction=not args.keep_archives,
                progress_str=progress_str)
        else:
            download_repo_by_clone(
                repo_info,
                remove_dotgit=not args.keep_dotgit,
                progress_str=progress_str)

if __name__ == "__main__":
    main()
