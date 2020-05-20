module DocumentUri = struct
  type t = string [@@deriving yojson]
end

module Position = struct
  type t = {
    line: int;
    character: int
  }
  [@@deriving yojson]
end

module Range = struct
  type t = {
    start: Position.t;
    end_: Position.t [@key "end"]
  }
  [@@deriving yojson]
end

module Location = struct
  type t = {
    uri: DocumentUri.t;
    range: Range.t
  }
  [@@deriving yojson]
end

module TextDocumentIdentifier = struct
  type t = {
    uri: DocumentUri.t
  }
  [@@deriving yojson]
end

module Diagnostic = struct
  module Severity = struct
    type t = Error | Warning | Information | Hint

    let yojson_of_t = function
        Error -> `Int 1
      | Warning -> `Int 2
      | Information -> `Int 3
      | Hint -> `Int 4

    let t_of_yojson = function
        `Int 1 -> Error
      | `Int 2 -> Warning
      | `Int 3 -> Information
      | `Int 4 -> Hint
      | json -> Json.error "Invalid diagnostic severity" json
  end
  type t = {
    range: Range.t;
    severity: Severity.t option; [@yojson.option]
    code: int option; [@yojson.option]
    source: string option; [@yojson.option]
    message: string
  }
  [@@deriving yojson] [@@yojson.allow_empty_fields]
end

module TextDocumentParam = struct
  type t = {
    uri: DocumentUri.t;
    languageId: string;
    version: int;
    text: string
  }
  [@@deriving yojson]
end

module VersionedTextDocumentIdentifier = struct
  type t = {
    uri: DocumentUri.t;
    version: int option
  }
  [@@deriving yojson]
end

(* Capabilities *)
module CompletionClientCapabilities = struct
  type completionItem = {
    snippetSupport: bool option; [@yojson.option]
    commitCharacterSupport: bool option; [@yojson.option]
    deprecatedSupport: bool option; [@yojson.option]
    preselectSupport: bool option; [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  type t = {
    dynamicRegistration: bool option; [@yojson.option]
    completionItem: completionItem option; [@yojson.option]
    contextSupport: bool option [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module DefinitionClientCapabilities = struct
  type t = {
    dynamicRegistration: bool option; [@yojson.option]
    linkSupport: bool option [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module PublishDiagnosticsClientCapabilities = struct
  type t = {
    relatedInformation: bool option; [@yojson.option]
    versionSupport: bool option; [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module ClientCapabilities = struct
  type t = {
    completion: CompletionClientCapabilities.t option; [@yojson.option]
    definition: DefinitionClientCapabilities.t option; [@yojson.option]
    publishDiagnostics: PublishDiagnosticsClientCapabilities.t option [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

(* Params *)

module InitializeParams = struct
  type t = {
    processId: int option;
    rootUri: DocumentUri.t option;
    clientInfo: ClientCapabilities.t
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module TextDocumentPositionParams = struct
  type t = {
    textDocument: TextDocumentIdentifier.t;
    position: Position.t
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module DefinitionParams = struct
  include TextDocumentPositionParams
end

module CompletionParams = struct
  module Context = struct
    module TriggerKind = struct
      type t = Invoked | TriggerCharacter | TriggerForIncompleteCompletions

      let yojson_of_t = function
        | Invoked -> `Int 1
        | TriggerCharacter -> `Int 2
        | TriggerForIncompleteCompletions -> `Int 3

      let t_of_yojson = function
        | `Int 1 -> Invoked
        | `Int 2 -> TriggerCharacter
        | `Int 3 -> TriggerForIncompleteCompletions
        | json -> Json.error "Invalid completion trigger kind" json
    end

    type t = {
      triggerKind: TriggerKind.t;
      triggerCharacter: string option [@yojson.option]
    }
    [@@deriving yojson]
  end

  type t = {
    textDocument: TextDocumentIdentifier.t;
    position: Position.t;
    context: Context.t option; [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module PublishDiagnosticsParams = struct
  type t = {
    uri: DocumentUri.t;
    version: int option; [@yojson.option]
    diagnostics: Diagnostic.t list
  }
  [@@deriving yojson]
end

module DidOpenParams = struct
  type t = {
    textDocumentItem: TextDocumentIdentifier.t
  }
  [@@deriving yojson]
end

module DidChangeParams = struct
  module TextDocumentContentChangeEvent = struct
    type t = {
      text: string
    }
    [@@deriving yojson]
  end

  type t = {
    textDocument: VersionedTextDocumentIdentifier.t;
    contentChanged: TextDocumentContentChangeEvent.t
  }
  [@@deriving yojson]
end

module DidCloseParams = struct
  type t = {
    textDocument: TextDocumentIdentifier.t
  }
  [@@deriving yojson]
end
