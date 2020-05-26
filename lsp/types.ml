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
    uri: DocumentUri.t;
  }
  [@@deriving yojson]
end

module TextEdit = struct
  type t = {
    range: Range.t;
    newText: string
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
    message: string
  }
  [@@deriving yojson] [@@yojson.allow_empty_fields]
end

module TextDocumentItem = struct
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

module TextDocumentSyncdKind = struct
  type t = None | Full | Incremental

  let yojson_of_t = function
    None -> `Int 0
  | Full -> `Int 1
  | Incremental -> `Int 2

  let t_of_yojson = function
    `Int 0 -> None
  | `Int 1 -> Full
  | `Int 2 -> Incremental
  | json -> Json.error "Invalid sync kind" json
end

module CompletionItem = struct
  type t = {
    label: string;
    detail: string option; [@yojson.option]
  }
  [@@deriving yojson]
end

module CompletionOptions = struct
  type t = {
    triggerCharacters: char list option; [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module SignatureHelpOptions = struct
  type t = Value
  let yojson_of_t _ = `Assoc []
  let t_of_yojson = function
      `Assoc _ -> Value
    | json -> Json.error "Invalid json for signature help option" json
end

module FormattingOptions = struct
  type t = {
    tabSize: int;
    insertSpaces: bool
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

(* Capabilities *)
module CompletionClientCapabilities = struct
  type completionItem = {
    snippetSupport: bool; [@default false]
    commitCharacterSupport: bool; [@default false]
    deprecatedSupport: bool; [@default false]
    preselectSupport: bool [@default false]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  type t = {
    dynamicRegistration: bool; [@default false]
    completionItem: completionItem option; [@yojson.option]
    contextSupport: bool; [@default false]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module DefinitionClientCapabilities = struct
  type t = {
    dynamicRegistration: bool; [@default false]
    linkSupport: bool [@default false]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module PublishDiagnosticsClientCapabilities = struct
  type t = {
    relatedInformation: bool; [@default false]
    versionSupport: bool; [@default false]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module TextDocumentClientCapabilities = struct
  type t = {
    completion: CompletionClientCapabilities.t option; [@yojson.option]
    definition: DefinitionClientCapabilities.t option; [@yojson.option]
    publishDiagnostics: PublishDiagnosticsClientCapabilities.t option [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module ClientCapabilities = struct
  type t = {
    textDocument: TextDocumentClientCapabilities.t option [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module ServerCapabilities = struct
  type t = {
    textDocumentSync: TextDocumentSyncdKind.t option; [@yojson.option]
    completionProvider: CompletionOptions.t option; [@yojson.option]
    signatureHelp: SignatureHelpOptions.t option; [@yojson.option]
    declarationProvider: bool option; [@yojson.option]
    definitionProvider: bool option; [@yojson.option]
    renameProvider: bool option; [@yojson.option]
    foldingRangeSupport: bool option; [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

(* Params *)

module InitializeParams = struct
  type t = {
    processId: int option;
    rootUri: DocumentUri.t option;
    capabilities: ClientCapabilities.t
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
    textDocument: TextDocumentItem.t
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
    contentChanges: TextDocumentContentChangeEvent.t list
  }
  [@@deriving yojson]
end

module DidCloseParams = struct
  type t = {
    textDocument: TextDocumentIdentifier.t
  }
  [@@deriving yojson]
end

module DocumentFormattingParams = struct
  type t = {
    textDocument: TextDocumentIdentifier.t;
    options: FormattingOptions.t
  }
  [@@deriving yojson]
end

(* Results *)
module InitializeResult = struct
  module ServerInfo = struct
    type t = {
      name: string;
      version: string option; [@yojson.option]
    }
    [@@deriving yojson]
  end

  type t = {
    capabilities: ServerCapabilities.t option; [@yojson.option]
    serverInfo: ServerInfo.t option [@yojson.option]
  }
  [@@deriving yojson]
end
