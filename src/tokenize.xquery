(:
  Copyright 2012 The Alpheios Project, Ltd.
  http://alpheios.net

  This file is part of Alpheios.

  Alpheios is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Alpheios is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 :)

(:
  tokenize API (based on LLT tokenize API)
      merging (boolean)
      shifting (boolean)
      indexing (boolean)
      splitting (boolean)
      enclitics_marker (string)
      tags (string)
      root (string)
      text (string)
      uri (uri)
      lang (string)
 :)

import module namespace request="http://exist-db.org/xquery/request";

declare namespace tei="http://www.tei-c.org/ns/1.0";

(: sets of characters by language :)
(: non-text characters :)
declare variable $s_nontext :=
(
  element nontext
  {
    attribute lang { "grc" },
    " “”—&quot;‘’,.:;&#x0387;&#x00B7;?!\[\]{}\-"
  },
  element nontext
  {
    attribute lang { "ara" },
    " “”—&quot;‘’,.:;?!\[\]{}\-&#x060C;&#x060D;"
  },
  element nontext
  {
    attribute lang { "*" },
    " “”—&quot;‘’,.:;&#x0387;&#x00B7;?!\[\](){}\-"
  }
);
(: characters which signify word break and are part of word :)
declare variable $s_breaktext :=
(
  element breaktext
  {
    attribute lang { "grc" },
    "᾽"
  },
   element breaktext
  {
    attribute lang { "ara" },
    "᾽"
  },
  element breaktext
  {
    attribute lang { "*" },
    "᾽"
  }
);

declare variable $s_tbPunc :=
(
  element punc
  {
    attribute lang { "*" },
    ",.:;\-—"
  }
);


(:
  Process set of nodes
 :)
declare function local:process-nodes(
  $a_nodes as node()*,
  $a_in-text as xs:boolean,
  $a_id as xs:string,
  $a_match-text as xs:string,
  $a_match-nontext as xs:string,
  $a_match-punc as xs:string) as node()*
{
  (: for each node :)
  for $node at $i in $a_nodes
  return
  typeswitch ($node)
    (:
      if element, copy and process all child nodes
     :)
    case element()
    return
    element { QName(namespace-uri($node),name($node)) }
    {
      local:process-nodes(
        $node/(node()|@*),
        ($a_in-text or (local-name($node) eq "body")) 
        and not(local-name($node) = ("note", "head", "speaker")),
        concat($a_id, "-", $i),
        $a_match-text,
        $a_match-nontext,
        $a_match-punc)
    }

    (: if text in body, process it else just copy it :)
    case $t as text()
    return
    if ($a_in-text)
    then
        (: splitting on new lines is a hack to avoid a stack overflow in eXist for large
           chunks of uninterrupted texts
        :)
        for $s at $k in tokenize($t,'\n')
        return
            local:process-text(normalize-space($s),
                               concat($a_id, "-", $k, '-', $i),
                               1,
                               $a_match-text,
                               $a_match-nontext,
                               $a_match-punc)
    else
      $node

    (: otherwise, just copy it :)
    default
    return $node
};

(:
  Process text string in body
 :)
declare function local:process-text(
  $a_text as xs:string,
  $a_id as xs:string,
  $a_i as xs:integer,
  $a_match-text as xs:string,
  $a_match-nontext as xs:string,
  $a_match-punc as xs:string) as node()*
{
  
  (: if anything to process :)
  if (string-length($a_text) > 0)
  then
    (: see if it starts with text :)
    let $is-text := matches($a_text, $a_match-text)
    
     (: see if it starts with text :)
    let $is-punc := matches($a_text, $a_match-punc)

    (: get initial text/non-text string :)
    let $t := replace($a_text,
                      if ($is-text) then $a_match-text else if ($is-punc) then $a_match-punc else $a_match-nontext,
                      "$1")

    return
    (
      (: return w element with text or non-text string :)      
      if ($is-text)
      then
        element {QName("http://www.tei-c.org/ns/1.0","w")}
            {
              (: assign position to n-attribute :)
              attribute n { $a_i },
              $t
            }
        else if ($is-punc)
        then
            element {QName("http://www.tei-c.org/ns/1.0","pc")}
            {
             (: assign position to n-attribute word :)
              attribute n { $a_i },
              $t
             }
        else
          text { $t },
    
      (: then recursively process rest of text :)
      local:process-text(substring-after($a_text, $t),
                         $a_id,
                         $a_i + 1,
                         $a_match-text,
                         $a_match-nontext,
                         $a_match-punc)
    )
  else ()
};

let $e_uri := replace(request:get-parameter('uri',''),' ','%20')
let $e_lang := request:get-parameter('lang','')
let $doc := doc($e_uri)
let $lang := if ($e_lang) then $e_lang else $doc//tei:text/@xml:lang

let $nontext :=
  if ($s_nontext[@lang eq $lang])
  then
    $s_nontext[@lang eq $lang]/text()
  else
    $s_nontext[@lang eq "*"]/text()
let $breaktext :=
  if ($s_breaktext[@lang eq $lang])
  then
    $s_breaktext[@lang eq $lang]/text()
  else
    $s_breaktext[@lang eq "*"]/text()
let $punc :=
    $s_tbPunc[@lang eq "*"]/text()
    
let $match-text :=
  concat("^([^", $nontext, $breaktext, "]+",
         if ($breaktext) then concat("[", $breaktext, "]?") else (),
         ").*")
let $match-nontext := concat("^([", $nontext, "]+).*")

let $match-punc := concat("^([", $punc, "]+).*")

return 
<doc>
    {local:process-nodes($doc/node(), false(), "w1", $match-text, $match-nontext, $match-punc)}
</doc>
   
