---
title: "Migration: tests and examples to unified types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:36.774859+02:00"
blocks:
  - habu-migration-libs-to-4e798110
  - habu-migration-tools-to-d4e8fcf8
  - habu-tests-migrate-declaration-40df313e
  - habu-tests-migrate-constructor-1b93b862
  - habu-tests-migrate-engine-a126c2a7
  - habu-tests-migrate-artifact-dbc892bb
  - habu-tests-retire-auxiliary-67147cae
---

Rewrite test, examples, and benchmark source fixtures to STRUCTURE and payload-capable ENUM, including source strings, golden diagnostics, package constructor spellings, negative declarations, AOT fixtures, and bootstrap inputs. Preserve intentional hard-rejection fixtures for removed keywords in one dedicated legacy-syntax suite. Run run-files coverage, full engine/type suites, property tests, examples, and benchmark compile checks.
