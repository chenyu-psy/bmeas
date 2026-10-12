"""Read-only validation and fail-closed GitHub Release publication helpers."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tarfile


def api(path, method="GET", data=None, binary=False, missing=False):
    args = ["gh", "api", path, "--method", method]
    if binary:
        args += ["-H", "Accept: application/octet-stream"]
    if data is not None:
        args += ["--input", "-"]
    result = subprocess.run(args, input=None if data is None else json.dumps(data).encode(),
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode:
        if missing and "(HTTP 404)" in result.stderr.decode():
            return None
        raise RuntimeError(result.stderr.decode())
    return result.stdout if binary else json.loads(result.stdout or b"null")


def admit(event, event_name, repository):
    """A manual verification can build, but can never authorize publication."""
    if event_name == "workflow_dispatch":
        sha = event.get("inputs", {}).get("sha", "")
        if not re.fullmatch("[0-9a-f]{40}", sha):
            raise ValueError("Verification requires an exact 40-character SHA")
        return sha, False
    if event.get("action") != "closed":
        raise ValueError("Only a closed pull request event may publish")
    pr = event["pull_request"]
    if not pr["merged"]:
        return "", False
    if (pr["base"]["ref"] != "main" or pr["head"]["ref"] != "develop" or
            not pr["head"]["repo"] or pr["head"]["repo"]["full_name"] != repository or
            pr["base"]["repo"]["full_name"] != repository):
        raise ValueError("Publication requires a same-repository develop -> main merge")
    sha = pr["merge_commit_sha"]
    if not re.fullmatch("[0-9a-f]{40}", sha):
        raise ValueError("Invalid merge commit SHA")
    return sha, True


def validate_artifact(directory, expected_sha):
    directory = Path(directory)
    metadata = json.loads((directory / "release.json").read_text())
    version = metadata["version"]
    if not re.fullmatch(r"(0|[1-9][0-9]*)(\.(0|[1-9][0-9]*)){2,3}", version):
        raise ValueError("Invalid artifact version")
    if metadata["sha"] != expected_sha or not re.fullmatch("[0-9a-f]{40}", expected_sha):
        raise ValueError("Artifact commit differs from the admitted commit")
    name = f"bmeas_{version}.tar.gz"
    archive = directory / name
    digest = hashlib.sha256(archive.read_bytes()).hexdigest()
    if metadata["file"] != name or metadata["sha256"] != digest:
        raise ValueError("Artifact name or checksum mismatch")
    if (directory / "SHA256SUMS").read_text() != f"{digest}  {name}\n":
        raise ValueError("Checksum manifest mismatch")
    with tarfile.open(archive) as package:
        description = package.extractfile("bmeas/DESCRIPTION").read().decode()
    if not re.search(r"^Package: bmeas$", description, re.M) or not re.search(
            r"^Version: " + re.escape(version) + "$", description, re.M):
        raise ValueError("Built package metadata mismatch")
    parts = version.split(".")
    development = len(parts) == 4 and int(parts[3]) >= 9000
    if metadata["prerelease"] is not development:
        raise ValueError("Development version must be marked prerelease")
    return metadata, archive


def publish(directory, repository, expected_sha, request=api):
    metadata, archive = validate_artifact(directory, expected_sha)
    prefix = f"repos/{repository}"
    version = metadata["version"]
    ref = request(f"{prefix}/git/ref/tags/{version}", missing=True)
    if ref is not None:
        obj = ref["object"]
        for _ in range(4):
            if obj["type"] == "commit":
                break
            if obj["type"] != "tag":
                raise ValueError("Existing tag is not a commit/tag")
            obj = request(f"{prefix}/git/tags/{obj['sha']}")["object"]
        if obj["type"] != "commit" or obj["sha"] != expected_sha:
            raise ValueError("Existing tag points to a different commit; refusing overwrite")
    else:
        request(f"{prefix}/git/refs", "POST", {"ref": f"refs/tags/{version}", "sha": expected_sha})
    marker = f"Commit: {expected_sha}\nSHA256: {metadata['sha256']}"
    release = request(f"{prefix}/releases/tags/{version}", missing=True)
    if release is None:
        body = (f"bmeas {version}\n\n{marker}\n\n"
                "Install the source tar with install.packages(path, repos=NULL, type='source').\n"
                "Experimental development builds do not establish scientific validity. "
                "Formal Bayes factors and density MCSE are not implemented.")
        release = request(f"{prefix}/releases", "POST", {
            "tag_name": version, "target_commitish": expected_sha,
            "name": f"bmeas {version}", "body": body, "draft": True,
            "prerelease": metadata["prerelease"], "make_latest": "false"})
    elif marker not in release.get("body", "") or release["prerelease"] != metadata["prerelease"]:
        raise ValueError("Existing release has different metadata; refusing overwrite")
    assets = {asset["name"]: asset for asset in request(f"{prefix}/releases/{release['id']}/assets?per_page=100")}
    for path in [archive, Path(directory) / "SHA256SUMS", Path(directory) / "release.json"]:
        if path.name in assets:
            content = request(f"{prefix}/releases/assets/{assets[path.name]['id']}", binary=True)
            if hashlib.sha256(content).digest() != hashlib.sha256(path.read_bytes()).digest():
                raise ValueError(f"Existing {path.name} differs; refusing overwrite")
        else:
            if not release["draft"]:
                raise ValueError("Published release has missing assets; inspect manually")
            subprocess.run(["gh", "release", "upload", version, str(path), "--repo", repository], check=True)
    verified = request(f"{prefix}/releases/{release['id']}/assets?per_page=100")
    assets = {asset["name"]: asset for asset in verified}
    for path in [archive, Path(directory) / "SHA256SUMS", Path(directory) / "release.json"]:
        content = request(f"{prefix}/releases/assets/{assets[path.name]['id']}", binary=True)
        if hashlib.sha256(content).digest() != hashlib.sha256(path.read_bytes()).digest():
            raise ValueError("Uploaded release asset verification failed")
    if release["draft"]:
        request(f"{prefix}/releases/{release['id']}", "PATCH", {
            "draft": False, "prerelease": metadata["prerelease"],
            "make_latest": "false" if metadata["prerelease"] else "true"})
    print("Release verified:", version, expected_sha)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", choices=["admit", "verify", "publish"])
    parser.add_argument("--directory", default="release-dist")
    args = parser.parse_args()
    if args.mode == "admit":
        sha, allowed = admit(json.loads(Path(os.environ["GITHUB_EVENT_PATH"]).read_text()),
                             os.environ["GITHUB_EVENT_NAME"], os.environ["GITHUB_REPOSITORY"])
        with open(os.environ["GITHUB_OUTPUT"], "a") as output:
            output.write(f"sha={sha}\npublish={str(allowed).lower()}\n")
    elif args.mode == "verify":
        metadata, _ = validate_artifact(args.directory, os.environ["BUILD_SHA"])
        print("Checked artifact only; no publication:", metadata["version"], metadata["sha"])
    else:
        event = json.loads(Path(os.environ["GITHUB_EVENT_PATH"]).read_text())
        sha, allowed = admit(event, os.environ["GITHUB_EVENT_NAME"], os.environ["GITHUB_REPOSITORY"])
        if not allowed or sha != os.environ["BUILD_SHA"]:
            raise ValueError("This event cannot publish")
        publish(args.directory, os.environ["GITHUB_REPOSITORY"], sha)
