#!/usr/bin/env python3

"""Download top GitHub repositories (by stars) in a given language."""

import os
import subprocess
import types
import sys
import github
import requests

N_TO_DOWNLOAD = 1000
LANGUAGE = "TeX"
GITHUB_TOKEN = None

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

def get_repo(repo, remove_after_extraction=False, progress_str=None):
    """Download the GitHub repository `repo`."""

    if progress_str is not None:
        progress_formatted = " ({})".format(progress_str)
    else:
        progress_formatted = ""

    if os.path.isdir(repo.extracted_path):
        message("Skipping already downloaded{}: {}".format(
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

    os.makedirs(os.path.dirname(repo.archive_path), exist_ok=True)
    download_file("GET", repo.tarball_url, repo.archive_path, download_progress)

    message("Extracting{}: {}".format(progress_formatted, repo.name))

    os.makedirs(repo.extracted_path, exist_ok=True)
    extract_tar(repo.archive_path, repo.extracted_path, strip_components=1)

    if remove_after_extraction:
        os.remove(repo.archive_path)

def main():
    """Main entry point."""

    message("Getting search results")

    gh = github.Github(GITHUB_TOKEN)
    repos = gh.search_repositories(
        "stars:>0 language:{}".format(LANGUAGE), sort="stars", order="desc")

    to_download = []
    for repo in repos[:N_TO_DOWNLOAD]:
        message(
            "Getting search results ({}/{})".format(
                len(to_download) + 1, N_TO_DOWNLOAD),
            overwrite_prev_line=True)

        to_download.append(types.SimpleNamespace(
            name=repo.full_name,
            tarball_url=repo.get_archive_link("tarball"),
            archive_path="archives/{owner} {name}.tar.gz".format(
                owner=repo.owner.login, name=repo.name),
            extracted_path="extracted/{owner} {name}".format(
                owner=repo.owner.login, name=repo.name)))

    for i_repo, repo_info in enumerate(to_download):
        get_repo(
            repo_info,
            progress_str="{}/{}".format(i_repo + 1, len(to_download)))

if __name__ == "__main__":
    main()
