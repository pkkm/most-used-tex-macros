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

def download_file(method, url, file, **kwargs):
    """Download a file without loading the entire response into memory."""
    # E.g. download_file("GET", "http://google.com", "google.html")

    response = requests.request(method, url, stream=True, **kwargs)
    response.raise_for_status() # Throw an error for bad status codes.

    with open(file, "wb") as handle:
        for block in response.iter_content(1024 * 1024):
            handle.write(block)

def extract_tar(archive, directory, strip_components=0):
    """Extract `archive` into `directory`."""
    args = ["tar", "xf", archive, "--directory={}".format(directory)]
    if strip_components > 0:
        args.append("--strip-components={}".format(strip_components))
    subprocess.run(args, check=True)
    # An alternative would be to use the `tarfile` module, but it has worse
    # security (it may extract outside the given directory).

def download_repo(repo, remove_after_extraction=False):
    """Download the GitHub repository `repo`."""
    if os.path.isdir(repo.extracted_path):
        print("Skipping (already downloaded): {}".format(repo.name))
        return
    print("Downloading: {}".format(repo.name))

    os.makedirs(os.path.dirname(repo.archive_path), exist_ok=True)
    download_file("GET", repo.tarball_url, repo.archive_path)

    os.makedirs(repo.extracted_path, exist_ok=True)
    extract_tar(repo.archive_path, repo.extracted_path, strip_components=1)

    if remove_after_extraction:
        os.remove(repo.archive_path)

def print_over_prev_line(message):
    """Print `message`, overwriting the previous line in the terminal."""
    if sys.stdout.isatty():
        sys.stdout.write("\x1b[1F\x1b[2K" + message + "\n")
        # Escape sequences: go to first character of previous line, erase line.
    else:
        print(message)

def main():
    """Main entry point."""

    gh = github.Github()
    repos = gh.search_repositories(
        "stars:>0 language:{}".format(LANGUAGE), sort="stars", order="desc")

    print("Getting search results")
    to_download = []
    for repo in repos[:N_TO_DOWNLOAD]:
        print_over_prev_line("Getting search results ({} of {})".format(
            len(to_download) + 1, N_TO_DOWNLOAD))

        to_download.append(types.SimpleNamespace(
            name=repo.full_name,
            tarball_url=repo.get_archive_link("tarball"),
            archive_path="archives/{owner} {name}.tar.gz".format(
                owner=repo.owner.login, name=repo.name),
            extracted_path="extracted/{owner} {name}".format(
                owner=repo.owner.login, name=repo.name)))

    for repo_info in to_download:
        download_repo(repo_info)

if __name__ == "__main__":
    main()
