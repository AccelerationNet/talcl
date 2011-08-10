# TALCL: A libary that implements the TAL template language for common lisp 

This library is a significant branch of the TAL implementation found in arnesi / ucw

## Examples
See the examples directory for runable lisp code and tal templates.

## Primary Goals
 * To make working with templates (particularly XML/HTML) simple and
   support designer type folks working on them.

 * To be more flexible, easier to use, easier to debug, and with more
   helpful error messages than the original TAL from arnesi and ucw.
   TAL now has a single execution environment (the common lisp env)
   rather than its own private tal environment.

 * integrate well with cxml dom & our buildnode library.  We want to
   be able to embed template content into our dom document in an
   efficient manner.  Currently these are stored as processing
   instructions in the dom, and in serialization we play out the sax
   events we buffered when processing the template

## Changes From Arnesi / UCW tal
 * Available without loading arnesi, yaclml, or ucw

 * A single environment (tal environment is used only to pass values as
   parameters); TAL processing occurs in the standard common lisp runtime.
   TAL package determines what is currently available.

 * Fixed inconsistencies about "$var" in non tal tags vs "var" in tal
   tags. $var should now work everywhere whether or not the tag or
   attribute is tal.
 
 * More consistancy in evaluation rules

 * More handlers, all handlers have tests
  * tal:def handler for defining new sub templates

 * <!-- --> XML Comments in the source template are rendered into the
   output

 * tal expression strings work in the body of a tag as well as
   attributes

 * $$ escapes to a single $ sign
 * $ evaluates one expression
  * $var and ${var} returns the value of the var
  * $(some-fn var) - calls some-fn with the value of var in the current
    package / env and splices the return into the doc
  * ${nil} doesnt print anything

 * DOM nodes and lists / vectors of dom-nodes can be spliced into the
   template EG: <span>$some-dom-content</span> will correctly put the
   dom content into the template

## TAL Handlers 
TAL handlers are tags / attributes that tal processes in a special way.

### Handler Descriptions
These are the descriptions of each of the tal handlers that are available.
This output was generated by the <tal:describe-handlers /> tag
```
NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::DESCRIBE-HANDLERS
  [symbol]

DESCRIBE-HANDLERS names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    TAG-HANDLER: Describes (in an comment), all tags and attribute handlers that are known
       (in *tal-attribute-handlers* or *tal-tag-handlers*)
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::DESCRIBE-HANDLER
  [symbol]

DESCRIBE-HANDLER names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    TAG-HANDLER: Describes (in an comment) the tag or attribute handler named by the
    attribute tal:name
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::PRINT-HANDLERS
  [symbol]

PRINT-HANDLERS names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) (VALUES CONS &OPTIONAL))
  Documentation:
    TAG-HANDLER: Lists (in a comment) the tag and attribute handlers
    that are currently registered.
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::PRINT-ENV
  [symbol]

PRINT-ENV names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    TAG-HANDLER: prints as a comment information about the
    tal-environment where this tag was encountered
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE:DEF
  [symbol]

DEF names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) (VALUES NULL &OPTIONAL))
  Documentation:
    TAG-HANDLER: creates an inline sub-template available for the duration
    of the templates execution.
    
    The template should be named by the tal:name attribute or by
    tal:name-expression if it should be evaluated.
    
    
    Example:
       <tal:def tal:name='deffed-template'>
           <div>
             <span tal:when='$selected'>*</span>
             <span tal:content='$label'</span>
           </div>
       </tal>
    
    tal:def can also be used to create string constants when used with tal:type=\"string\"
    
    Example:
       <tal:def tal:name='deffed-string' tal:type='string'>This is my string</tal>
    
      results in a let binding (deffed-string \"This is my string\")
    

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE:INCLUDE
  [symbol]

INCLUDE names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    TAG-HANDLER: includes another template at this point in the file.
    The template should be named by the tal:name attribute or by
    tal:name-expression if it should be evaluated.
    
    The path to the referenced template should be relative to the location
    of the template it is included in.
      
    The environment can be extended by including attributes in the
    parameter namespace, or immediate children in the parameter namespace.
    
    Example:
       <tal:include tal:name-expression='$tal-name' param:foo='foo' />
       <tal:include tal:name='other-template.tal' param:foo='foo'>
         <param:contents>
           <div>
             <span tal:when='$selected'>*</span>
             <span tal:content='$label'</span>
           </div>
         </param:contents>
       </tal>
    
    The template other-template.tal will be evaluated with the additional
    parameters of 'foo' and 'contents'.

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE:LOOP
  [symbol]

LOOP names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    TAG-HANDLER: Loop across a list and repeat the children. On each
      iteration the environment is extended with the value
    
      tal:list should evaluate to a list
      tal:constant-list is parsed as a comma separated string
    
      <tal:loop tal:var='class' tal:list='$efficiencies'>
        <label class='map-control-item'>
          <img class='marker-icon ${$class}' />
          <span class='text' tal:content='$class'/>
          <input type='checkbox' name='filter-efficiency' item-type='${$class}' />
        </label>
      </tal:loop>
    
    assuming that $efficiencies resolves to the list {foo,bar}.
        <label class='map-control-item'>
          <img class='marker-icon foo' />
          <span class='text' tal:content='foo'/>
          <input type='checkbox' name='filter-efficiency' item-type='foo' />
        </label>
        <label class='map-control-item'>
          <img class='marker-icon bar' />
          <span class='text' tal:content='bar'/>
          <input type='checkbox' name='filter-efficiency' item-type='bar' />
        </label>

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::LISP
  [symbol]

LISP names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    TAG-HANDLER: evaluate the body of the tag as lisp code.
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::WITHOUT-READER
  [symbol]

WITHOUT-READER names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE:TAL
  [symbol]

TAL names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE:IN-PACKAGE
  [symbol]

IN-PACKAGE names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    ATTRIBUTE-HANDLER: sets the package in which lisp evaluation
    happens.
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::LET
  [symbol]

LET names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    ATTRUBTE-HANDLER: Extend environment with a given list of bindings,
    as for LET form.
    
    Example:
    <div tal:let='foo 3'><div tal:content='$foo'/></div>
    
    Goes to: <div><div>3</div></div>

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::UNLESS
  [symbol]

UNLESS names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    ATTRIBUTE-HANDLER: Causes this tag to only appear when
    the evaluated value of the attribute is nil.

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE:WHEN
  [symbol]

WHEN names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    ATTRIBUTE-HANDLER: Causes this tag to only appear when
    the evaluated value of the attribute is not nil.

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE:REPLACE
  [symbol]

REPLACE names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    ATTRIBUTE-HANDLER: Replaces the content of the tag with the
    evaluated value of the attribute. Whether the content is escaped
    depends upon the sibling attribute 'tal:escape-html
    
    Example:
    <div tal:replace='$foo' class='jack'/>
    
    Gets output as:
      |Whatever was in $foo|

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::CONTENT-AS-IS
  [symbol]

CONTENT-AS-IS names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    ATTRIBUTE-HANDLER:
    Replaces the content of the tag with the evaluated value of the
    attribute. This tag does not escape the content.
    
    Example:
    <div tal:content-as-is='$foo' class='jack'/>
    Gets output as
    <div class='jack'>
      |Whatever was in $foo|
    </div>

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE:CONTENT
  [symbol]

CONTENT names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Documentation:
    ATTRIBUTE-HANDLER:
    Replaces the content of the tag with the evaluated value of the
    attribute. This tag escapes its content to be html-safe.
    
    Example:
    <div tal:content='$foo' class='jack'/>
    Gets output as
    <div class='jack'>
      |Whatever was in $foo|
    </div>

  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp

NET.COMMON-LISP.PROJECT.BESE.TAL.CORE::TALABLE-STRINGS
  [symbol]

TALABLE-STRINGS names a compiled function:
  Lambda-list: (TAG)
  Derived type: (FUNCTION (T) *)
  Source file: /home/ACCELERATION/russ/lisp/talcl/src/handlers.lisp
```


;; Copyright (C) 2011 Acceleration.net, Russ Tyndall
;;   email: bobbysmith007@gmail.com
;;
;; Copyright (c) 2002-2006, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; - Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;; of its contributors may be used to endorse or promote products derived
;; from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

