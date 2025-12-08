;;;; habu-mcp.asd - ASDF system for Habu MCP Server
;;;;
;;;; Modular MCP server with tools in separate files.

(asdf:defsystem #:habu-mcp
  :description "MCP Server for Habu Lisp Compiler"
  :version "5.0"
  :serial t
  :depends-on (#:habu)
  :components ((:file "mcp-base")
               (:module "tools"
                :components ((:file "crash-analyze")))))
