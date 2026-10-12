"""Offline checks: no GitHub credentials, requests, publication or package fits."""
import ast
from contextlib import redirect_stdout
from copy import deepcopy
import hashlib
import io
import json
from pathlib import Path
import tarfile
import tempfile
import unittest

from automation import admit, classify_source, publish, validate_artifact

SHA = "a" * 40
REPOSITORY = "chenyu-psy/bmeas"


class AutomationBoundaries(unittest.TestCase):
    def setUp(self):
        self.event = {"action": "closed", "pull_request": {
            "merged": True, "merge_commit_sha": SHA,
            "base": {"ref": "main", "repo": {"full_name": REPOSITORY}},
            "head": {"ref": "develop", "repo": {"full_name": REPOSITORY}}}}
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name)
        self.name = "bmeas_0.0.47.9000.tar.gz"
        with tarfile.open(self.directory / self.name, "w:gz") as archive:
            content = b"Package: bmeas\nVersion: 0.0.47.9000\n"
            member = tarfile.TarInfo("bmeas/DESCRIPTION")
            member.size = len(content)
            archive.addfile(member, io.BytesIO(content))
        digest = hashlib.sha256((self.directory / self.name).read_bytes()).hexdigest()
        self.metadata = {"version": "0.0.47.9000", "sha": SHA, "file": self.name,
                         "sha256": digest, "prerelease": True}
        self.write_metadata()
        (self.directory / "SHA256SUMS").write_text(f"{digest}  {self.name}\n")

    def write_metadata(self):
        (self.directory / "release.json").write_text(json.dumps(self.metadata))

    def test_configuration_package_and_removal_matrix(self):
        self.assertEqual(classify_source(False, False), "configuration")
        self.assertEqual(classify_source(True, False), "package")
        self.assertEqual(classify_source(True, True), "package")
        with self.assertRaises(ValueError):
            classify_source(False, True)

    def test_manual_verification_never_publishes(self):
        self.assertEqual(admit({"inputs": {"sha": SHA}}, "workflow_dispatch", REPOSITORY), (SHA, False))

    def test_wrong_source_or_fork_cannot_publish(self):
        for field, value in [("ref", "feature/test"), ("repo", {"full_name": "someone/bmeas"})]:
            event = deepcopy(self.event)
            event["pull_request"]["head"][field] = value
            with self.assertRaises(ValueError):
                admit(event, "pull_request", REPOSITORY)

    def test_unmerged_pr_does_not_publish(self):
        self.event["pull_request"]["merged"] = False
        self.assertEqual(admit(self.event, "pull_request", REPOSITORY), ("", False))

    def test_admitted_sha_and_artifact_must_match(self):
        validate_artifact(self.directory, SHA)
        with self.assertRaises(ValueError):
            validate_artifact(self.directory, "b" * 40)

    def test_development_cannot_be_marked_stable(self):
        self.metadata["prerelease"] = False
        self.write_metadata()
        with self.assertRaises(ValueError):
            validate_artifact(self.directory, SHA)

    def test_changed_archive_fails_before_remote_access(self):
        with open(self.directory / self.name, "ab") as output:
            output.write(b"changed")
        with self.assertRaises(ValueError):
            publish(self.directory, REPOSITORY, SHA,
                    request=lambda *a, **k: self.fail("Remote access before artifact validation"))

    def test_conflicting_tag_cannot_be_overwritten(self):
        calls = []
        def request(path, method="GET", **kwargs):
            calls.append(method)
            return {"object": {"type": "commit", "sha": "b" * 40}}
        with self.assertRaises(ValueError):
            publish(self.directory, REPOSITORY, SHA, request=request)
        self.assertEqual(calls, ["GET"])

    def test_matching_release_is_idempotent_and_changed_asset_fails(self):
        for changed in [False, True]:
            calls = []
            assets = [{"name": p.name, "id": i} for i, p in enumerate(
                [self.directory / self.name, self.directory / "SHA256SUMS", self.directory / "release.json"])]
            contents = {asset["id"]: (self.directory / asset["name"]).read_bytes() for asset in assets}
            if changed:
                contents[0] += b"different"
            def request(path, method="GET", **kwargs):
                calls.append(method)
                if "/git/ref/" in path:
                    return {"object": {"type": "commit", "sha": SHA}}
                if "/releases/tags/" in path:
                    return {"id": 1, "draft": False, "prerelease": True,
                            "body": f"Commit: {SHA}\nSHA256: {self.metadata['sha256']}"}
                if "/releases/assets/" in path:
                    return contents[int(path.rsplit("/", 1)[1])]
                return assets
            if changed:
                with self.assertRaises(ValueError):
                    publish(self.directory, REPOSITORY, SHA, request=request)
            else:
                with redirect_stdout(io.StringIO()):
                    publish(self.directory, REPOSITORY, SHA, request=request)
            self.assertTrue(all(method == "GET" for method in calls))


if __name__ == "__main__":
    for path in Path(__file__).parent.glob("*.py"):
        ast.parse(path.read_text(), filename=str(path))
    unittest.main()
