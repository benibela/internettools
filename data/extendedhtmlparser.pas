{**
  @abstract This units contains a template based html parser named THtmlTemplateParser

  @author Benito van der Zander (http://www.benibela.de)
}
unit extendedhtmlparser;
{
Copyright (C) 2008 - 2012 Benito van der Zander (BeniBela)
                          benito@benibela.de
                          www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils,simplehtmltreeparser,xquery,
   xquery__regex,
    bbutils;


type
//**@abstract These are all possible template commands, for internal use
//**@value tetIgnore useless thing
//**@value tetHTMLOpen normal html opening tag, searched in the processed document
//**@value tetHTMLClose normal html closing tag, searched in the processed document
//**@value tetHTMLText text node, , searched in the processed document
//**@value tetCommandMeta <template:meta> command to specify how strings are compared
//**@value tetCommandMetaAttribute <template:meta-attribute> command to specify how attributes are compared
//**@value tetCommandRead <template:read> command to set a variable
//**@value tetCommandShortRead <template:s> command to execute a xq expression
//**@value tetCommandLoopOpen <template:loop> command to repeat something as long as possible
//**@value tetCommandIfOpen <template:if> command to skip something
//**@value tetCommandElseOpen <template:else> command to skip something
//**@value tetCommandSwitchOpen <template:switch> command to branch
//**@value tetCommandSwitchPrioritizedOpen <template:switch-prioritized> command to branch
//duplicate open/close because this simplifies the case statements
TTemplateElementType=(tetIgnore,
                      tetHTMLOpen, tetHTMLClose,
                      tetHTMLText,
                      tetMatchElementOpen, tetMatchElementClose,
                      tetMatchText,
                      tetCommandMeta, tetCommandMetaAttribute, tetCommandRead, tetCommandShortRead,
                      tetCommandLoopOpen,tetCommandLoopClose,
                      tetCommandIfOpen, tetCommandIfClose,
                      tetCommandElseOpen, tetCommandElseClose,
                      tetCommandSwitchOpen, tetCommandSwitchClose,
                      tetCommandSwitchPrioritizedOpen, tetCommandSwitchPrioritizedClose,
                      tetCommandSiblingsOpen, tetCommandSiblingsClose,
                      tetCommandSiblingsHeaderOpen, tetCommandSiblingsHeaderClose
                      );
TTemplateElementFlag = (tefOptional, tefSwitchChild);
TTemplateElementFlags = set of TTemplateElementFlag;

(*TNotifyCallbackFunction = procedure () of object;
TVariableCallbackFunction = procedure (variable: string; value: string) of object;
TReadCallbackFunction = procedure (read: pchar; readLen:longint) of object;*)

//**Possible callback for getting the value of a variable
TReplaceFunction = procedure (variable: string; var value:string) of object;

ETemplateParseException = class(Exception);
EHTMLParseException = class(Exception);

{ EHTMLParseMatchingException }

EHTMLParseMatchingException = class(EHTMLParseException)
  sender: TObject;
  constructor create(const mes: string; const asender: TObject);
  function partialMatches: string;
end;

THtmlTemplateParser=class;

TStringAttributeList = tStringList;

{ TTemplateElement }
//**@abstract Interally used template tree element @exclude
TTemplateElement=class(TTreeNode)
  //constant template
  templateType: TTemplateElementType;
  flags: TTemplateElementFlags;
  templateAttributes: tStringAttributeList;

  //matching information
  contentRepetitions: integer;
  match: TTreeNode; //this is only for template debugging issues (it will be nil iff the element was never matched, or the iff condition never satisfied)

  //"caches"
  test, condition, valuepxp, source, min, max, varname, ignoreSelfTest: IXQuery; //when adding, remember to update all the references
  textRegexs: array of TWrappedRegExpr;
  data: tobject;

  function templateReverse: TTemplateElement; inline;
  function templateNext: TTemplateElement; inline;

  procedure setTemplateAttribute(name, avalue: string);

  constructor create; override;
  constructor create(attyp: TTemplateElementType);
  procedure postprocess(parser: THtmlTemplateParser);
  procedure assign(asource: TTreeNode); override;
  procedure initializeCaches(parser: THtmlTemplateParser; recreate: boolean = false);
  procedure freeCaches;
  destructor destroy;override;
end;


//** Specifies when the text of text nodes is trimmed. Each value removes strictly more whitespace than the previous ones.
//** @value ttnNever never, all whitespace is kept
//** @value ttnForMatching When comparing two text nodes, whitespace is ignored; but all whitespace will be returned when reading text
//** @value ttnAfterReading The XQ-functions like ., text(), deep-text() return the text trimmed, but the whitespace is still stored in the tree (so deep-text returns whitespace between child nodes)
//** @value ttnWhenLoading All starting/ending whitespace is unconditionally removed from all text nodes
TTrimTextNodes = (ttnNever, ttnForMatching, ttnWhenLoadingEmptyOnly, ttnWhenLoading);

//** This specifies the handling of the variables read in the previous document @br@br
//** @value kpvForget Old variables are deleted @br
//** @value kpvKeepValues Old variables are moved from the property variableChangelog to the property oldVariableChangelog @br
//** @value kpvKeepInNewChangeLog Old variables stay where they are (i.e. in the variableChangelog property merged with the new ones)@br
//** In every case all node variables are converted to strings (because the nodes point to elements of the previous document, but the previous document will be deleted)
TKeepPreviousVariables = (kpvForget, kpvKeepValues, kpvKeepInNewChangeLog);

TXQTermVariableArray = array of TXQTermVariable;

{ THtmlTemplateParser }

(***
  @abstract This is the template processor class which can apply a template to one or more html documents.

  You can use it by calling the methods @code(parseTemplate) and @code(parseHTML). @code(parseTemplate) loads a certain template
  and @code(parseHTML) matches the template to a html/xml file.@br
  A template file is just like a html file with special commands. The parser than matches every text and tag
  of the template to text/tag in the html file, while ignoring every additional data in latter file.
  If no match is possible an exception is raised.@br
  The template can extract certain values from the html file into variables, and you can access these variables with the property @link(variables) and variableChangeLog.
  Former only contains the final value of the variables, latter records every assignment during the matching of the template.@br@br


  @bold(Getting started)


  Creating a template to analyze a xml-file/webpage:

  @orderedList(

  @item(First, you should remove all things from the webpage that are uninteresting, dynamically generated or invalid xml (or alternatively start with an empty file as template).)

  @item(Then, you should replace all parts that you want to extract with @code(<t:s>yourVariableName:=text()</t:s>).@br
        This will write the value of the text node that contains the t:s tag in the variable yourVariableName.@br@br
        Instead of the @code(t:s) tag, you can also use the short notation @code({yourVariableName:=text()}); and instead of
        @code(text()) to read the text node, you can also use @code(@attrib) to read an attribute; or an arbitrary complex
        @link(xquery.TXQueryEngine xpath/xquery-expression))

  @item(Then the template is finished, at least the trivial things)
  )

  If you want to read several elements like table rows, you need to surround the matching tags with template:loop, e.g. @code(<template:loop><tr>..</tr></template:loop>)
  and the things between the loop-tags is repeated as long as possible. You can also use the short notation by adding a star like @code(<tr>..</tr>* ).@br


  Using the templates from Pascal:


  @orderedList(
  @item(First, create a new THtmlTemplateParser: @code(parser := THtmlTemplateParser.create()))
  @item(Load the template with  @code(parser.parseTemplate('..template..')) or  @code(parser.parseTemplateFile('template-file')))
  @item(Process the webpage with  @code(parser.parseHTML('..html..')) or  @code(parser.parseHTMLFile('html-file')))
  @item(Read the result of variable yourVariableName through parser.variables.values['yourVariableName'])
  )

  If you used loops, only the last value of the variable is available in the variables property, the previous values can
  be enumerated through variableChangelog.

  @bold(Template examples)

  @definitionList(

  @itemLabel(@italic(Example, how to read first <b>-tag):)
  @item(
    Html-File: @code(<b>Hello World!</b>)@br
    Template: @code(<b>{.}</b>)@br

  This will set the default variable @code(_result) to @code("Hello World!" )
  )

  @itemLabel(@italic(Example, how to read the first <b>-tag in a explicit named variable):)
  @item(
    Html-File: @code(<b>Hello World!</b>)@br
    Template: @code(<b>{$test}</b>)@br

  This will set the variable @code(test) to @code("Hello World!"). @br
  Some alternative forms are @code(<b>{$test := .}</b>), @code(<b><t:s>test := .</t:s></b>), @code(<b><template:s>test := text()</template:s></b>) or @code(<b><t:read var="test" source="text()"></b>).
  )

  @itemLabel(@italic(Example, how to read all <b>-tags:))
  @item(
    Html-File: @code(<b>Hello </b><b>World!</b>)@br
    Template: @code(<b>{.}</b>* )@br

  This will change the value of the variable @code(_result) twice, to @code("Hello " ) and @code("World!"). Both values are available in the variable changelog.@br
  Some alternative forms are: @code(<t:loop><b>{.}</b></t:loop>), @code(<template:loop><b>{.}</b></template:loop>), @code(<template:loop><b>{_result := text()}</b></template:loop>), ...
  )

  @itemLabel(@italic(Example, how to read the first field of every row of a table):)
  @item(
    Html-File: @code(<table> <tr> <td> row-cell 1 </td> </tr> <tr> <td> row-cell 2 </td> </tr> ... <tr> <td> row-cell n </td> </tr> </table>)@br
    Template: @code(<table> <template:loop> <tr> <td> {$field} </td> </tr> </template:loop> </table>)@br

    This will read row after row, and will write each first field to the change log of the variable @code(field).
  )

  @itemLabel(@italic(Example, how to read several fields of every row of a table):)
  @item(
    Html-File: @code(<table> <tr> <td> a </td> <td> b </td> <td> c </td> </tr> ... </tr> </table>)@br
    Template: @code(<table> <template:loop> <tr> <td> {$field1} </td> <td> {$field2} </td> <td> {$field3} </td> ... </tr> </template:loop> </table>)@br

    This will read @code($field1=a, $field2=b, $field3=c)...@br
    If you now want to process multiple pages which have a similar, but slightly different table/data layount, you can create
    a template for each of them, and the Pascal side of the application is independent of the source pages.
    Then it is even possible for the user of the application to add new pages.
  )

  @itemLabel(@italic(Example, how to read all elements between two elements):)
  @item(
    Html-File:
@preformatted(
  <h1>Start</h1>
    <b>Text 1</b>
    <b>Text 2</b>
  <h1>End</h1>)@br
    Template: @preformatted(
  <h1>Start</h1>
    <b>{.}</b>*
  <h1>End</h1>
)@br

   This will read all b elements between the two headers.

  )

  @itemLabel(@italic(Example, how to read the first list item starting with an unary prime number):)
  @item(
  Html-File: @code(... <li>1111: this is 4</li><li>1:1 is no prime</li><li>1111111: here is 7</li><li>11111111: 8</li> ...)@br
  Template: @code(<li template:condition="filter(text(), '1*:') != filter(text(), '^1?:|^(11+?)\1+:')">{$prime}</li>)@br

  This will return "1111111: here is 7", because 1111111 is the first prime in that list.)


  )

  See the unit tests in tests/extendedhtmlparser_tests.pas for more examples.



  @bold(Why not XPath/CSS-Selectors?)@br

  You might wonder, why you should use templates, if you already know XPath or CSS Selectors.

  The answer is that, although XPath/CSS works fine for single values, it is not
  powerful enough to read multiple values or data from multiple sources, because:

  @unorderedList(
    @item(XPath/CSS expressions are not able to return multiple values.

          Each expression can only return a single node set, so if you need to read m different values from n different pages,
           you need O(m * n) expressions, while you only need O(n) templates.
       @br For example, if you need to read a table listing objects and 2 values for each of them, like in this table:
       @br   @code(<table><tr><td>name</td><td>value 1</td><td>value 2</td></tr></table>)
       @br you can use this template:
       @br   @code(<table><tr><td>{$name}</td><td>{$value1}</td><td>{$value2}</td></tr>*</table>)
       @br and get three arrays with the needed values.
       @br With XPath you would need three expressions: @br
           @code( names := ... //table/tr/td[1] ...;   @br
                  values1 := ... //table/tr/td[2] ...; @br
                  values2 := ... //table/tr/td[3] ...;
            )

           Or CSS: @br
            @code( names := ... table tr td:nth-child(1) ...;    @br
                   values1 := ... table tr td:nth-child(2) ...;  @br
                   values2 := ... table tr td:nth-child(3) ...;
             )

          )


    @item( XPath is not suited to process html.

           XPath was made to process xml not html, so there are some important functions missing.
      @br  One of the most common actions of web scraping is to select (e.g. div) elements based on their classes.
           Novices think this can be written as @code(//div[@class = "foobar"]), but this is wrong, because the class attribute can list multiple classes.
           And the correct XPath expression @code(//div[contains(concat(" ", @class, " "), " foobar ")]) is very ugly.
      @br  Templates know the semantic meaning of attributes, so you can just use @code(<div class="foobar"></div>).
      @br  Normal XPath is also case-sensitive, while html is case-insensitive, so if the expression works at all, depends on the parser changing the case of all tags.

           You might see this as a reason to use CSS selectors, but:
          )

    @item(CSS Selectors are not able to process the data

          CSS only selects the elements and cannot change their values.
      @br E.g. if you need to parse numbers from two pages, one of them using the Amercian format 123,456.00 and the other one the
          European format 123.456,00, you cannot use CSS selectors to parse them both without changing something in the host language.
      @br With  templates you can use  @code({.}) and @code({translate(., ".,", ",.")}) and are done.
          )


    @item(Templates can be written much faster.

           Because you do not need to write them at all and instead just copy them from the input page.
       @br E.g. in the example above to create a template for the webpage @code(<table><tr><td>name</td><td>value 1</td><td>value 2</td></tr></table>)
           you just need to insert some @code({}* ) and get the complete template @code(<table><tr><td>{$name}</td><td>{$value1}</td><td>{$value2}</td></tr>*</table>).
       @br To get the XPath-expressions @code(/table/tr/td[1,2,3]) you actually need to look at the structure of the page.

           Of course the table example is trivial, only on more complex examples you can see how powerful the templates actually are:

           Let us assume the data is not nicely packed in a table, but contained in a formatted text, like:

           @code(
             <b>name a</b>: value-a1, value-a2<br>        @br
             <b>name b</b>: value-b1, value-b2<br>
             ...
           )

           The template is a little bit more complex, since you need to split the values:

           @code(<t:loop><b>{$name}</b>: <t:s>value1 := extract(text(), ":(.+),", 1), value2 := extract(text(), ":(.+),(.+)", 2)</t:s><br/></t:loop>)

       @br However, if you want to solve this task with XPath 1.0 or CSS, you will discover that it is impossible.
           CSS can not select the text nodes at all, and XPath 1 cannot split them.
       @br The best you can manage is to select the values with XPath and then split them in the host language, but then you cannot parse multiple different sources by swapping the expressions.
       @br And although XPath 2 or 3 can split the values, it becomes rather ugly:

       @code( names := //b,                                                                 @br
              values1 := //b/substring-after(following-sibling::text()[1], ":")             @br
              values2 := //b/substring-after(following-sibling::text()[1], ",")
            )

       Another example is if you just need the data from a part of the page, e.g. between two headers like here.

@preformatted(
  not needed
  ...
  <h1>Header 1</h1>

    <b>name a</b>: value-a1, value-a2<br>
    <b>name b</b>: value-b1, value-b2<br>

  <h1>Header 2</h1>
  ...
  not needed
)

       The template change is trivial, you just add both headers to the template:

          @code(<h1>Header 1</h1>                                                                                                                             @br
                <t:loop><b>{$name}</b>: <t:s>value1 := extract(text(), ":(.+),", 1), value2 := extract(text(), ":(.+),(.+)", 2)</t:s><br></t:loop>            @br
                <h1>Header 2</h1>)

       How to do it in XPath? (in XPath 2, it is of course still impossible with XPath 1)


       Well, it gets just crazy:


        @code( names := //h1[. = "Header 1"]/following-sibling::b[following-sibling::h1[1] = "Header 2"],                                                              @br
               values1 := //h1[. = "Header 1"]/following-sibling::b[following-sibling::h1[1] = "Header 2"]/substring-after(following-sibling::text()[1], ":")          @br
               values2 := //h1[. = "Header 1"]/following-sibling::b[following-sibling::h1[1] = "Header 2"]/substring-after(following-sibling::text()[1], ",")
             )

          )

    @item(Multiple XPath/CSS expressions are not adaptable to changes

             If the page layout changes, you need to rewrite all the expressions.
             With templates, you just need to apply the local change.

             E.g. if you want to get multiple data from the last div on this page:

@preformatted(<div id="foobar">
   ...
   <div class="abc">...</div>
   <div>
     <b> .. data 1 .. </b>
     <i> .. data 2 .. </i>
   </div>
</div>
)

             The template would be

@preformatted(<div id="foobar">
   <div class="abc"/>
   <div>
     <b>{$data1}</b>
     <i>{$data2}</i>
   </div>
</div>
             )

             If you do it with XPath, you have two expressions:

             @code(
                     data1 := ... //div[@id="foobar"]/div[@class = "abc"]/following-sibling::div/b ...          @br
                     data2 := ... //div[@id="foobar"]/div[@class = "abc"]/following-sibling::div/i ...
             )

             Now, if the page layout is changed to e.g.

@preformatted(<div id="foobar">
   ...
   <div class="def">...</div>
   <div>
     ...
   </div>
</div>
)

             You get a diff

             @code(
               - <div class="abc">...</div>             @br
               + <div class="abc">...</div>
             )

             Which can basically be applied directly to the template and leads to:

@code(<div id="foobar">
   <div class="def"/>
   <div>
     <b>{$data1}</b>
     <i>{$data2}</i>
   </div>
</div>
)

             But using XPath expressions, you need to change multiple expressions and you have to look at
             each expression to find the correct div class to change:

             @code(
                     data1 := ... //div[@id="foobar"]/div[@class = "def"]/following-sibling::div/b ...          @br
                     data2 := ... //div[@id="foobar"]/div[@class = "def"]/following-sibling::div/i ...
             )
             .
       )

       @item(XPath/CSS cannot handle errors

            XPath/CSS do not provide any information in case the query fails.

            E.g. if you use @code(//table[@id="foobar"]/tr) to get all rows of a table, and it returns 0 rows,
            you do not know, if the table was actually empty, or if the page layout changed and the table does not
            exist anymore, or if you use a new html parser, which inserts (correctly) a tbody element between the table and tr.

            But if you use a template @code(<table id="foobar"><tr>{.}*</tr></table>) and it returns anything,
            it is guaranteed that the table exists, since it raises an exception in case it is missing.
       )

       @item(Metapher: XPath/CSS are like string functions, templates are like regular expressions

       If you write XPath/CSS expressions you give an explicit list of instructions, i.e. you write @code(/foo) to get all foo-children,
       you write @code([bar]) to filter all elements that have a bar child, you write @code(..) to get the parent, you write
       @code([position() <= 10]) to take the first ten elements...

       This is exactly the same concept, as if you write e.g. @code(copy(s, pos(s, 'foo'), 10) ) to find the 'foo' substring and
       then take the next 10 characters.

       But you would never do that nowadays, if you can use a regular expression like @code('foo(.{1})').

       Such a regular expression now implicitely selects the characters after foo, just like a template @code(<foo/>{text()})
       selects the text after a foo-element.

       )


  )

  That said, it is obviously also possible to use XPath or CSS with the templates:

  @code(<html>{//your/xpath/expression}</html>) or @code(<html>{css("your.css#expression")}</html>)

  In fact there exists no other modern XPath/CSS interpreter for FreePascal.


  @bold(Template reference)

  Basically the template file is a html file, and the parser tries to match the structure of the template html file to the html file. @br
  A tag of the html file is considered as equal to a tag of the template file, if the tag names are equal, all attributes are the same (regardless of their order) and every child node of the tag in the template is also equal to a child node of the tag in the html file (in the same order and nesting).@br
  Text nodes are considered as equal, if the text in the html file starts with the whitespace trimmed text of the template file. All comparisons are performed case insensitive.@br
  The matching occurs with backtracking, so it will always find the first and longest match.

  The following template commands can be used:
   @unorderedList(
      @item(@code(<template:read var="??" source="??" [regex="??" [submatch="??"]]/>)
        @br The @link(xquery.TXQueryEngine XPath-expression) in source is evaluated and stored in variable of var.
        @br If a regex is given, only the matching part is saved. If submatch is given, only the submatch-th match of the regex is returned. (e.g. b will be the 2nd match of "(a)(b)(c)") (However, you should use the xq-function extract instead of the regex/submatch attributes, because former is more elegant)
        )
      @item(@code(<template:s>var:=source</template:s>)
        @br Short form of @code(template:read). The expression in @code(source) is evaluated and assigned to the variable @code(s). @br You can also set several variables like @code(a:=1,b:=2,c:=3) (Remark: The := is actually part of the expression syntax, so you can use much more complex expressions.)
        )
      @item(@code(<template:if test="??"/>  .. </template:if>)
        @br Everything inside this tag is only used iff the XPath-expression in test equals to true)
      @item(@code(<template:else [test="??"]/>  .. </template:else>)
        @br Everything inside this tag is only used iff the immediate previous if/else block was not executed. @br
            You can chain several else blocks that have test attributes together after an starting if, to create an ifelse chain, in which
            only one if or else block is used.@br
            E.g.: @code(<template:if test="$condition">..</template:if><template:else test="$condition2">..</template:else><template:else>..</template:else>) )
      @item(@code(<template:loop [min="?"] [max="?"]>  .. </template:loop>)
        @br Everything inside this tag is repeated between [min,max] times. (default min=0, max=infinity)
        @br E.g. if you write @code(<template:loop>  X </template:loop> ), it has the same effect as XXXXX with the largest possible count of X <= max for a given html file.
        @br If min=0 and there is no possible match for the loop interior the loop is completely ignored.
        @br If there are more possible matches than max, they are ignored.
        )
      @item(@code(<template:switch [value="??"]> ... </template:switch>)
      This command can be used to match only one of several possibilities. It has two different forms:
      @orderedList(
       @item(Case 1: All direct child elements are template commands:@br
          Then the switch statement will choose the first child command, whose attribute @code(test) evaluates to true.
          @br Additionally, if one of the child elements has an attributes @code(value), the expressions of the switch and the child @code(value) attribute are evaluated, and the command is only choosen, if both expressions are equal.
          @br An element that has neither a @code(value) nor a @code(test) attribute is always choosen (if no element before it is choosen).
          @br If no child can be choosen at the current position in the html file, the complete switch statement will skipped.
       )
       @item(Case 2: All direct child elements are normal html tags:@br
        @br This tag is matched to an html tag, iff one of its direct children can be matched to that html tag.
        @br For example @code(<template:switch><a>..</a> <b>..</b></template:switch>) will match either @code(<a>..</a>) or @code(<b>..</b>), but not both. If there is an <a> and a <b> tag in the html file, only the first one will be matched (if there is no loop around the switch tag).
            These switch-constructs are mainly used within a loop to collect the values of different tags, or to combine to different templates.
        @br If no child can be matched at the current position in the html file, the matching will be tried again at the next position (different to case 1).
       )
      ))
      @item(@code(<template:switch prioritized="true"> ... </template:switch>)
        Another version of a case 2 switch statement that only may contain normal html tags. @br
        The switch-prioritized prefers earlier child element to later child elements, while the normal switch match alls child elements equally. So a normal switch containing <a> and <b>, will match <a> or <b>, whichever appears first in the html file.
        The switch-prioritized contrastingly would match <a>, if there is any <a>, and <b> only iff there is no <a> in the html file. @br
        Therefore @code(<template:switch-prioritized [value="??"]> <a>..</a> <b>..</b> .. </template:switch-prioritized>) is identical to
        @code(<a template:optional="true">..<t:s>found:=true()</t:s></a> <b template:optional="true" template:test="not($found)">..<t:s>found:=true()</t:s></b> ...).@br
        (this used to be called @code(<template:switch-prioritized>), which is still supported, but will be removed in future versions)
      )
      @item(@code(<template:match-text [matches=".."] [starts-with=".."] [ends-with=".."] [contains=".."] [eq=".."] [case-sensitive=".."] [list-contains=".."]/>)@br
        Matches a text node and is more versatile than just including the text in the template.@br
        @code(matches) matches an arbitrary regular expression against the text node. @br
        @code(starts-with/ends-with/contains/eq) check the text verbatim against the text node, in the obvious way.@br
        list-contains treats the text of the node as a comma separated list and tests if that list contains the attribute value .@br
        case-sensitive enables case-sensitive comparisons.@br
        (older versions used regex/is instead matches/eq, which is now deprecated and will be removed in future versions)
      )
      @item(@code(<template:element> .. </template:element>)@br
        Matches any element. @br
        It is handled like an element without t: prefix, but skips the name test. E.g. if either @code(<a>) or @code(<b>) should be allowed, you can use @code(<t:element t:condition="name() = ('a', 'b')">) rather than listing both.
      )
      @item(@code(<template:siblings-header [id=".."]> .. </template:siblings-header> <template:siblings [id=".."]>..<template:siblings>)@br
        These two commands connect elements in different parts of the matched document, and reorder the elements in the siblings command to the same order matched by the header command. The children of these two commands are associated by their order, i.e. the first child of the header command is associated with the first child of the sibling (replay) command.
        For example, the columns of a table can be associated with the columns in the table header, such the pattern can match any order of those columns. @code(<table><thead><t:siblings-header><th>myheader1</th><th>myheader2</th>..</t:siblings-header></thead> <tr><t:siblings><td>..</td><td>..</td></t:siblings></tr></table> ). Here you can swap the two columns in the document without affecting the matching (except for the order of the output variables). @br
        The command allows loop counters and optional elements in the header. If an header element can match multiple headers in the document the corresponding element in the replay command is duplicated accordingly. @br
        Sibling elements in the header that cannot be matched by the pattern are ignored. Similarly additional elements are ignored during the replay, but the elements of the document ignored are not necessarily in the same order in both cases, since the sibling command only reorders its children. E.g. if you use the above example to match table columns and apply it to a document with an additional column between the two headers, the sibling command will still match the first two columns of the table (unless the second @code(<td>) cannot match the new column, but in this case you might want to use t:switch ).
        You can put an additional @code( <th/>* ) and @code( <td/> ) at the end of the header/replay command to prevent this. The th will match any unexpected column and the td will skip it during replay. @br
        It is currently not backtracked and the header command will swallow all the siblings it can. If it has swallowed too many, the matching will fail, even if it could have succeeded, if the header had skipped some optional elements.
      )
      @item(@code(<template:meta [text-matching="??"] [case-sensitive="??"] [attribute-text-matching="??"] [attribute-case-sensitive="??"]/>) @br
        Specifies meta information to change the template semantic:@br
        @code(text-matching): specifies how text node in the template are matched against html text nodes. You can set it to the allowed attributes of match-text. (default is "starts-with") @br
        @code(text-case-sensitive): specifies if text nodes are matched case sensitive. @br
        @code(attribute-matching): like @code(text-matching) for the values of attribute nodes (note that is currently affecting all attributes in the template. future versions will only change it for following elements) @br
        @code(attribute-case-sensitive): like @code(text-case-sensitive) for the values of attribute nodes (note that is currently affecting all attributes in the template. future versions will only change it for following elements) @br
      )
      @item(@code(<template:meta-attribute [name="??"] [text-matching="??"]  [case-sensitive="??"]) @br
        Like meta for all attributes with a certain name.
      )
    )@br
    These template attributes can be used on any template element:
    @unorderedList(
      @item(@code(template:test="xpath condition") @br
        The element (and its children) is ignored if the condition does not evaluate to true (so @code(<template:tag test="{condition}">..</template:tag>) is a short hand for @code(<template:if test="{condition}">@code(<template:tag>..</template:tag></template:if>))).
      )
      @item(@code(template:ignore-self-test="xpath condition") @br
        The element (and NOT its children) is ignored if the condition does not evaluate to true.
      )
    )
    @br
    On html/matching tags also these matching modifying attributes can be used:
    @unorderedList(
      @item(@code(template:optional="true") @br if this is set the file is read successesfully even if the tag doesn't exist.@br
                                               You should never have an optional element as direct children of a loop, because the loop has lower priority as the optional element, so the parser will skip loop iterations if it can find a later match for the optional element.
                                               But it is fine to use optional tags that have an non-optional parent tag within the loop. )
      @item(@code(template:condition="xpath") @br if this is given, a tag is only accepted as matching, iff the given xpath-expression returns true (powerful, but slow) @br
                                                      (condition is not the same as test: if test evaluates to false, the template tag is ignored; if condition evaluates to false, the html tag is not found)
      )
    )

    The default prefixes for template commands are "template:" and "t:", you can change that with the templateNamespace-property or by defining a new namespace in the template like @code(xmlns:yournamespace="http://www.benibela.de/2011/templateparser" ). (only the xmlns:prefix form is supported, not xmlns without prefix)


    @bold(Short notation)

    Commonly used commands can be abbreviated as textual symbols instead of xml tags. To avoid conflicts with text node matching, this short notation is only allowed at the beginning of template text nodes.

    The short read tag @code(<t:s>foo:=..</t:s>) to read something in variable @code(foo) can be abbreviated as @code({foo:=..}). Similarly {} can be written within attributes to read the attribute, e.g. @code(<a href="{$dest := .}"/>).@br
    Also the trailing @code(:= .) can be omitted, if only one variable assignment occurs, e.g. as @code({$foo}) is equivalent to @code(foo := .) and @code($foo := .).

    Optional and repeated elements can be marked with ?, *, +, {min, max}; like @code(<a>?...</a>) or, equivalent, @code(<a>..</a>?). @br
    An element marked with ? becomes optional, which has the same effect as adding the template:optional="true" attribute.@br
    An element marked with * can be repeated any times, which has the same effect as surrounding it with a template:loop element.@br
    An element marked with + has to be repeated at least once, which has the same effect as surrounding it with a template:loop element with attribute min=1.@br
    An element marked with {min,max} has to be repeated at least min-times and at most max-times (just like in a t:loop) (remember that additional data/elements are always ignored).@br
    An element marked with {count} has to be repeated exactly count-times (just like in a t:loop) (remember that additional data/elements are always ignored).@br


    @bold(Breaking changes from previous versions:)@br
    @unorderedList(
    @item(As was announced in planned changes, the meaning of {$x} and {6} was changed)
    @item(As was announced in planned changes, the meaning of <x value="{$x}"/> was changed)
    @item(Adding the short notation breaks all templates that match text nodes starting with *, +, ? or {)
    @item(The default template prefix was changed to template: (from htmlparser:). You can add the old prefix to the templateNamespace-property, if you want to continue to use it)
    @item(All changes mentioned in pseudoxpath.)
    @item(Also text() doesn't match the next text element anymore, but the next text element of the current node. Use .//text() for the old behaviour)
    @item(All variable names in the pxp are now case-sensitive in the default mode. You can set variableChangeLog.caseSensitive to change it to the old behaviour (however, variables defined with in the expression by @code(for/some/every) (but not by @code(:=) ) remain case sensitive))
    @item(There was always some confusion, if the old variable changelog should be deleted or merged with the new one, if you process several html documents. Therefore the old merging option was removed and replaced by the KeepPreviousVariables property.)
    )

    @bold(Planned breaking changes: )@br
    @unorderedList(
    @item(Avoid unmatched parenthesis and pipes within text nodes:@br
          Currently is no short notation to read alternatives with the template:switch command, like @code(<template:switch><a>..</a><b>..</b><c>..</c></template:switch>).@br
          In future this might be the same as @code((<a>..</a>|<b>..</b>|<c>..</c>)).@br
          )
    )

*)
THtmlTemplateParser=class
  protected
    //FObjects: boolean;
    FRepetitionRegEx: TWrappedRegExpr;
    FTrimTextNodes, lastTrimTextNodes: TTrimTextNodes;
    FVeryShortNotation: boolean;
    FUnnamedVariableName: string;

    FOutputEncoding: TSystemCodePage;
    FKeepOldVariables: TKeepPreviousVariables;

    FTemplate, FHTML: TTreeParser;
    FHtmlTree: TTreeNode;
    FQueryEngine: TXQueryEngine;
    FQueryContext: TXQEvaluationContext;

    FVariables,FVariableLog,FOldVariableLog,FVariableLogCondensed: TXQVariableChangeLog;
    FParsingExceptions, FSingleQueryModule: boolean;

    FAttributeDefaultCaseSensitive: boolean;
    FAttributeDefaultMatching: string;
    FAttributeMatching: TStringList;

    FSiblingMatching: array of record
      id: string;
      childrenCount: integer; //for pattern debugging
      order: array of integer;
    end;

    function GetVariableLogCondensed: TXQVariableChangeLog;
    function GetVariables: TXQVariableChangeLog;
    function getHTMLTree: TTreeNode;
    function getTemplateTree: TTreeNode;
    function GetTemplateNamespace: TNamespaceList;
    function GetTemplateHasRealVariableDefinitions: boolean;
    procedure GetTemplateRealVariableDefinitions(var vars: TXQTermVariableArray; out hasDefaultVariable: Boolean);
    function GetTemplateContextDependencies: TXQContextDependencies;
  protected
    FCurrentTemplateName: string; //currently loaded template, only needed for debugging (a little memory waste)
    //FCurrentStack: TStringList;
    //FOnVariableRead: TVariableCallbackFunction;

    //function readTemplateElement(status:TParsingStatus):boolean; //gibt false nach dem letzten zurück
    //function evaluateXQVariable(sender: TObject; const variable: string; var value: IXQValue): boolean;
    //procedure defineXQVariable(sender: TObject; const variable: string; const value: IXQValue);
    //procedure executeTemplateCommand(status:TParsingStatus;cmd: TTemplateElement;afterReading:boolean);
    //function getTemplateElementDebugInfo(element: TTemplateElement): string;

    function templateElementFitHTMLOpen(html:TTreeNode; template: TTemplateElement): Boolean;
    procedure resetAttributeMatching;
    function matchTemplateTree(htmlParent, htmlStart, htmlEnd:TTreeNode; templateStart, templateEnd: TTemplateElement): boolean;

    procedure initializeCaches;
  public
    procedure parseHTMLSimple(html, uri, contenttype: string); //**< Parses a HTML file without performing matching. For internal use,
    function matchLastTrees: Boolean;
  public
    constructor create;
    destructor destroy; override;


    procedure parseTemplate(template: string; templateName: string = '<unknown>');//**< loads the given template, stores templateName for debugging issues
    procedure parseTemplateFile(templatefilename: string); //**<loads a template from a file
    function parseHTML(html: string; htmlFileName: string = ''; contentType: string = ''):boolean; //**< parses the given data by applying a previously loaded template. htmlFileName is just for debugging issues
    function parseHTMLFile(htmlfilename: string):boolean; //**< parses the given file by applying a previously loaded template.
    //procedure addFunction(name:string;varCallFunc: TVariableCallbackFunction);overload;
    //procedure addFunction(name:string;notifyCallFunc: TNotifyCallbackFunction);overload;

    //**This replaces every $variable; in s with variables.values['variable'] or the value returned by customReplace (should not be used anymore)
    function replaceVarsOld(s:string;customReplace: TReplaceFunction=nil):string; deprecated;
    //**This treats str as extended string and evaluates the pxquery expression x"str"
    function replaceEnclosedExpressions(str:string):string;

    function debugMatchings(const width: integer): string;
    function debugMatchings(const width: integer; includeText: boolean; includeAttributes: array of string): string;
    function parseQuery(const expression: string): IXQuery; //**< Returns a IXQuery that accesses the variable storage of the template engine. Mostly intended for internal use, but you might find it useful to evaluate external XPath expressions which are not part of the template

    property variables: TXQVariableChangeLog read GetVariables;//**<List of all variables (variableChangeLog is usually faster)
    property variableChangeLog: TXQVariableChangeLog read FVariableLog; //**<All assignments to a variables during the matching of the template. You can use TStrings.GetNameValue to get the variable/value in a certain line
    property oldVariableChangeLog: TXQVariableChangeLog read FOldVariableLog; //**<All assignments to a variable during the matching of previous templates. (see TKeepPreviousVariables)
    property VariableChangeLogCondensed: TXQVariableChangeLog read GetVariableLogCondensed; //**< VariableChangeLog with duplicated objects removed (i.e. if you have obj := object(), obj.a := 1, obj.b := 2, obj := object(); the normal change log will contain 4 objects (like {}, {a:1}, {a:1,b:2}, {}), but the condensed log only two {a:1,b:2}, {})

    property templateNamespaces: TNamespaceList read GetTemplateNamespace; //**< Global namespaces to set the commands that will be recognized as template commands. Default prefixes are template: and t: @br Namespaces can also be defined in a template with the xmlns: notation and the namespace url  'http://www.benibela.de/2011/templateparser'
    property ParsingExceptions: boolean read FParsingExceptions write FParsingExceptions; //**< If this is true (default) it will raise an exception if the matching fails.
    property OutputEncoding: TSystemCodePage read FOutputEncoding write FOutputEncoding; //**< Output encoding, i.e. the encoding of the read variables. Html document and template are automatically converted to it
    property KeepPreviousVariables: TKeepPreviousVariables read FKeepOldVariables write FKeepOldVariables; //**< Controls if old variables are deleted when processing a new document (see TKeepPreviousVariables)
    property trimTextNodes: TTrimTextNodes read FTrimTextNodes write FTrimTextNodes; //**< How to trim text nodes (default ttnAfterReading). There is also pseudoxpath.XQGlobalTrimNodes which controls, how the values are returned.
    property UnnamedVariableName: string read FUnnamedVariableName write FUnnamedVariableName; //**< Default variable name. If a something is read from the document, but not assigned to a variable, it is assigned to this one. (Default: _result)
    property AllowVeryShortNotation: boolean read FVeryShortNotation write FVeryShortNotation; //**< Enables the the very short notation (e.g. {a:=text()}, <a>*) (default: true)
    property SingleQueryModule: boolean read FSingleQueryModule write FSingleQueryModule;  //**< If all XPath/XQuery expressions in the templates are kept in the same module. Only if true, XQuery variables/functions declared are accessible in other read commands. (declarations must be preceded by @code(xquery version "1.0";) and followed by an expression, if only @code(())) Global variables, declared with a simple $x := value, are always everywhere accessible. (default true)

    property hasRealVariableDefinitions: boolean read GetTemplateHasRealVariableDefinitions; //**< If the currently loaded template contains := variable definitions (contrary to assign values to the default variable with {.} )  (CAN ONLY BE USED AFTER the template has been applied!)

    property TemplateTree: TTreeNode read getTemplateTree; //**<A tree representation of the current template
    property HTMLTree: TTreeNode read getHTMLTree; //**<A tree representation of the processed html file
    property TemplateParser: TTreeParser read FTemplate; //**< X/HTML parser used to read the templates (public so you can change the parsing behaviour, if you really need it)
    property HTMLParser: TTreeParser read FHTML; //**< X/HTML parser used to read the pages (public so you can change the parsing behaviour, if you really need it)
    property QueryEngine: TXQueryEngine read FQueryEngine; //**< XQuery engine used for evaluating query expressions contained in the template
    property QueryContext: TXQEvaluationContext read FQueryContext write FQueryContext; //**< Context used to evaluate XQuery expressions. For internal use.

  end;

  TXQTerm_VisitorFindWeirdGlobalVariableDeclarations = class(TXQTerm_Visitor)
    vars: TXQTermVariableArray;
    listVars, findNestedVariables: boolean;

    hasVars: boolean;
    function visit(t: PXQTerm): TXQTerm_VisitAction; override;
  end;

//** xml compatible namespace url to define new template prefixes
const HTMLPARSER_NAMESPACE_URL = 'http://www.benibela.de/2011/templateparser';

type TExtractionKind = (ekAuto, ekXPath2, ekXPath3, ekPatternHTML, ekPatternXML, ekCSS, ekXQuery1, ekXQuery3, ekMultipage); //that is Xidel stuff, but used in simpleinternet as well. just ignore it
function guessExtractionKind(e: string): TExtractionKind;

implementation

uses math,strutils;

const //TEMPLATE_COMMANDS=[tetCommandMeta..tetCommandIfClose];
      firstRealTemplateType = tetMatchElementOpen;
      COMMAND_CLOSED:array[firstRealTemplateType..high(TTemplateElementType)] of longint=(1, 2, 0,0,0,0,0,1,2,1,2,1,2,1,2,1,2,1,2,1,2); //0: no children, 1: open, 2: close
      COMMAND_STR:array[firstRealTemplateType..high(TTemplateElementType)] of string=('element', 'element', 'match-text','meta','meta-attribute','read','s','loop','loop','if','if','else','else','switch','switch','switch-prioritized','switch-prioritized','siblings','siblings', 'siblings-header','siblings-header');


{ TTemplateElement }

function strToCommand(const ns, s:string; treeTyp: TTreeNodeType): TTemplateElementType;
var  t: TTemplateElementType;
begin
  if ((treeTyp = tetOpen) or (treeTyp = tetClose)) then begin
    if strEqual(ns, HTMLPARSER_NAMESPACE_URL) then begin
      for t:=low(COMMAND_STR) to high(COMMAND_STR) do
        if striequal(s,COMMAND_STR[t]) then begin
          if treeTyp = tetOpen then exit(t)
          else if COMMAND_CLOSED[t] = 0 then exit(tetIgnore)
          else if COMMAND_CLOSED[t] = 2 then exit(t);
        end;
      raise ETemplateParseException.Create('Unbekannter Templatebefehl: '+s);
    end;
  end;
  case treeTyp of
    tetOpen, tetDocument: exit(tetHTMLOpen);
    tetClose: exit(tetHTMLClose);
    tetText: exit(tetHTMLText);
    else result := tetIgnore; //should not happen
  end;
end;

function nodeToCommand(n: TTreeNode): TTemplateElementType; inline;
begin
  with n do begin
    result := strToCommand(getNamespaceURL(), value, typ);
    if (result = tetCommandSwitchOpen) and striEqual(getAttribute('prioritized'), 'true') then
      result := tetCommandSwitchPrioritizedOpen;
  end;
end;

procedure ignore(const {%H-}intentionallyUnusedParameter: TObject); inline; begin end;


type
  TCommandSiblingData = class
      id: integer;
      children: array of TTemplateElement;
  end;
  TCommandSiblingHeaderData = class(TCommandSiblingData)
    countChildren: array of TTemplateElement;
  end;


constructor EHTMLParseMatchingException.create(const mes: string; const asender: TObject);
begin
  inherited create(mes);
  sender := asender;
end;

function EHTMLParseMatchingException.partialMatches: string;
begin
  if sender is THtmlTemplateParser then result := THtmlTemplateParser(sender).debugMatchings(80)
  else result := '';
end;

function TTemplateElement.templateReverse: TTemplateElement;
begin
 result := TTemplateElement(reverse);
end;

function TTemplateElement.templateNext: TTemplateElement;
begin
  result := TTemplateElement(next);
end;

procedure TTemplateElement.setTemplateAttribute(name, avalue: string);
begin
 if templateAttributes = nil then templateAttributes := TStringList.Create;
 templateAttributes.Values[name] := avalue;
end;

constructor TTemplateElement.create;
begin

end;

constructor TTemplateElement.create(attyp: TTemplateElementType);
begin
  templateType:=attyp;
  if attyp < firstRealTemplateType then raise ETemplateParseException.Create('invalid type');
  if COMMAND_CLOSED[attyp] = 2 then typ := tetClose
  else typ := tetOpen;
  value := COMMAND_STR[attyp];
  hash := nodeNameHash(value);
end;

function isVariableName(s: string): boolean;
begin
  result := false;
  s := trim(s);
  if length(s) < 2 then exit();
  if s[1] <> '$' then exit;
  s[1] := ' ';
  s := StringsReplace(s, [' ',#9,#13,#10], ['','','',''], [rfReplaceAll]); //for dot notation
  if baseSchema.isValidQName(s) then exit(true);
  if strBeginsWith(s, 'Q{') then begin
    delete(s, 1, pos(s, '}'));
    if baseSchema.isValidNCName(s) then exit(true);
  end;
end;

procedure TTemplateElement.postprocess(parser: THtmlTemplateParser);
var
 curChild: TTreeNode;
 temp: TTemplateElement;
 i: Integer;
 rv: String;
begin
  //inherited initialized;
  if attributes <> nil then
    for i := attributes.Count - 1 downto 0 do
      if attributes.Items[i].getNamespaceURL() = XMLNamespaceUrl_XMLNS then
        attributes.delete(i);

  templateType:=nodeToCommand(self);

  if attributes <> nil then
    for i := attributes.Count - 1 downto 0 do begin
      rv := attributes.Items[i].realvalue;
      if (templateType in [firstRealTemplateType..high(TTemplateElementType)] - [tetMatchElementOpen])
         or (attributes.Items[i].getNamespaceURL() = HTMLPARSER_NAMESPACE_URL) then begin
        if templateAttributes = nil then templateAttributes := tStringAttributeList.Create;
        templateAttributes.Add(attributes.Items[i].value+'='+attributes.Items[i].realvalue);
        attributes.Delete(i);
      end else if  parser.AllowVeryShortNotation and (rv <> '') and (rv[1] = '{') and (rv[length(rv)] = '}') then begin
        temp := TTemplateElement.createElementPair('s') as TTemplateElement;
        temp.namespace := TNamespace.make(HTMLPARSER_NAMESPACE_URL, 't');
        temp.templateType:=tetCommandShortRead;
        temp.reverse.namespace := temp.namespace;
        temp.templateReverse.templateType:=tetIgnore;
        temp.addChild(TTemplateElement.create());
        temp.templateNext.typ := tetText;
        temp.templateNext.templateType := tetIgnore;
        rv := copy(rv, 2, length(rv) - 2);
        if isVariableName(rv) then temp.templateNext.value:= rv + ':= @'+attributes.Items[i].getNodeName()
        else temp.templateNext.value:='@'+attributes.Items[i].getNodeName() + ' / (' + rv +')';
        addChild(temp);
        if templateAttributes = nil then templateAttributes := TStringAttributeList.Create;
        //todo: optimize ?
        if templateAttributes.Values['condition'] = '' then templateAttributes.Values['condition'] := 'exists(@'+attributes.Items[i].getNodeName()+')'
        else templateAttributes.Values['condition'] := '(' + templateAttributes.Values['condition'] + ') and exists(@'+attributes.Items[i].getNodeName()+')';
        //attributes.Delete(i); // Items[i].realvalue := '';
        attributes.Delete(i); // Items[i].realvalue := '';
      end;
    end;

  if templateAttributes <> nil then
    if templateAttributes.Values['optional'] = 'true' then flags+=[tefOptional];

  if templateType = tetCommandShortRead then begin
    curChild := getFirstChild();
    while curChild <> nil do begin
      TTemplateElement(curChild).templateType:=tetIgnore;
      curChild := curChild.getNextSibling();
    end;
  end;

  if templateType = tetCommandSwitchOpen then begin
    curChild := getFirstChild();
    while curChild <> nil do begin
      TTemplateElement(curChild).flags+=[tefSwitchChild];
      curChild := curChild.getNextSibling();
    end;
  end;
end;

procedure TTemplateElement.assign(asource: TTreeNode);
var
  s: TTemplateElement;
  i: Integer;
begin
  inherited assign(asource);
  if asource is TTemplateElement then begin
    s := TTemplateElement(asource);
    templateType := s.templateType;
    flags := s.flags;
    if s.templateAttributes <> nil then begin
      templateAttributes := TStringAttributeList.Create;
      templateAttributes.Assign(s.templateAttributes);
    end;

    if s.data <> nil then begin
      if s.data is TCommandSiblingData then begin
        data := s.data.newinstance;
        TCommandSiblingData(data).id := TCommandSiblingData(s.data).id;
        TCommandSiblingData(data).children := TCommandSiblingData(s.data).children;
        SetLength(TCommandSiblingData(data).children, length(TCommandSiblingData(data).children));
      end;
      if s.data is TCommandSiblingHeaderData then begin
        TCommandSiblingHeaderData(data).countChildren := TCommandSiblingHeaderData(s.data).countChildren;
        SetLength(TCommandSiblingHeaderData(data).countChildren, length(TCommandSiblingHeaderData(data).countChildren));
      end;
    end;

    contentRepetitions:=s.contentRepetitions;
    match:=s.match;

    if s.test <> nil then test := s.test.clone;
    if s.condition <> nil then condition := s.condition.clone;
    if s.valuepxp <> nil then valuepxp := s.valuepxp.clone;
    if s.source <> nil then source := s.source.clone;
    if s.min <> nil then min := s.min.clone;
    if s.max <> nil then max := s.max.clone;
    if s.varname <> nil then varname := s.varname.clone;
    if s.ignoreSelfTest <> nil then ignoreSelfTest := s.ignoreSelfTest.clone;

    setlength(textRegexs, length(s.textRegexs));
    for i := 0 to high(textRegexs) do
      textRegexs[i] := wregexprClone(s.textRegexs[i]);
  end;
end;

type

TXQueryEngineBreaker = class(TXQueryEngine)
  function parserEnclosedExpressionsString(s: string): IXQuery;
end;

function TXQueryEngineBreaker.parserEnclosedExpressionsString(s: string): IXQuery;
begin
  result := parseXStringNullTerminated(s);
end;

procedure TTemplateElement.initializeCaches(parser: THtmlTemplateParser; recreate: boolean = false);
  function cachePXP(name: string): IXQuery;
  var i: integer;
  begin
    if templateAttributes = nil then exit(nil);
    i := templateAttributes.IndexOfName(name);
    if i < 0 then exit(nil);
    result := parser.parseQuery(templateAttributes.ValueFromIndex[i]);
  end;

  procedure cacheRegExpr(name: string; prefix, suffix: string; escape: boolean);
  var i: integer;
   r: String;
   cs: String;
   flags: TWrappedRegExprFlags;
  begin
    i := templateAttributes.IndexOfName(name);
    if i < 0 then exit();
    r := templateAttributes.ValueFromIndex[i];
    if parser.FTrimTextNodes <> ttnNever then begin
      prefix:=prefix+'\s*';
      suffix:='\s*'+suffix;
    end;
    if escape then r := prefix + strEscapeRegex(r) + suffix
    else r := prefix + r + suffix;
    SetLength(textRegexs, length(textRegexs) + 1);
    flags := [wrfSingleLine];
    i := templateAttributes.IndexOfName('case-sensitive');
    if i < 0 then include(flags, wrfIgnoreCase)
    else begin
      cs := templateAttributes.ValueFromIndex[i];
      if (cs = 'false') or (cs = 'case-insensitive') or (cs = 'insensitive') then include(flags, wrfIgnoreCase)
    end;
    textRegexs[high(textRegexs)] := wregexprParse(r, flags);
  end;

  procedure cacheSibling(header: boolean);
  var data: TCommandSiblingData;
    child: TTreeNode;
    count, i: Integer;
    id: String;
  begin
    count := getChildrenCount([tetOpen]);
    if count = 0 then raise ETemplateParseException.Create('Siblings need a child.');
    if header then begin
      data := TCommandSiblingHeaderData.Create;
      SetLength(TCommandSiblingHeaderData(data).countChildren, count);
    end else data := TCommandSiblingData.Create;
    SetLength(data.children, count);

    if templateAttributes = nil then id := ''
    else id := templateAttributes.Values['id'];
    data.id := -1;
    for i := 0 to high(parser.FSiblingMatching) do
      if parser.FSiblingMatching[i].id = id then begin
        data.id := i;
        if parser.FSiblingMatching[i].childrenCount <> count then raise ETemplateParseException.Create('t:siblings children count mismatch.');
        break;
      end;
    if data.id = -1 then begin
      SetLength(parser.FSiblingMatching, length(parser.FSiblingMatching) + 1 );
      data.id := high(parser.FSiblingMatching);
      parser.FSiblingMatching[data.id].id := id;
      parser.FSiblingMatching[data.id].childrenCount := count;
    end;

    child := getFirstChild();
    if child = nil then exit();
    count := 0;
    repeat
      if child.typ = tetOpen then begin
        data.children[count]:= child as TTemplateElement;
        if TTemplateElement(child).templateType = tetCommandLoopOpen then begin
          if not header then raise ETemplateParseException.Create('Loop not allowed in siblings');
          TCommandSiblingHeaderData(data).countChildren[count] := TTemplateElement(child);
          data.children[count]:= child.getFirstChild()  as TTemplateElement;
        end;
        inc(count);
      end;
      child := child.getNextSibling();
    until child = nil;


    if Self.data = nil then self.data := data //make it thread-safe?, but this is not thread-safe. should it be thread-safe?
    else data.free;
  end;

var
  temp: String;
begin
  contentRepetitions := 0;

  if recreate then freeCaches;

  case templateType of
    tetCommandSiblingsHeaderOpen: cacheSibling(true);
    tetCommandSiblingsOpen: cacheSibling(false);
  end;

  if (test <> nil) or (condition <> nil) or (valuepxp <> nil) or (source <> nil) or (length(textRegexs) > 0) then exit;

  if templateType = tetCommandShortRead then begin
    temp := deepNodeText();
    if isVariableName(temp) then temp += ' := .'; //replace $xx with $xx := .
    source := parser.parseQuery(temp); //todo: encoding?
  end else
    source := cachePXP('source');

  if templateAttributes= nil then exit;

  test := cachePXP('test');
  condition := cachePXP('condition');
  valuepxp := cachePXP('value');
  min := cachePXP('min');
  max := cachePXP('max');
  ignoreSelfTest := cachePXP('ignore-self-test');

  if (templateType = tetMatchText) then begin
    cacheRegExpr('matches', '', '', false);
    cacheRegExpr('regex', '', '', false); //deprecated
    cacheRegExpr('starts-with', '^', '.*$', true);
    cacheRegExpr('ends-with', '^.*', '$', true);
    cacheRegExpr('contains', '', '', true);
    cacheRegExpr('eq', '^', '$', true);
    cacheRegExpr('is', '^', '$', true); //deprecated
    cacheRegExpr('list-contains', '(^|,) *', ' *(,|$)', true);
  end else if (templateType = tetCommandRead) then begin
    cacheRegExpr('regex', '', '', false);
    if templateAttributes.IndexOfName('var') >= 0 then begin
      varname := parser.parseQuery('x"'+templateAttributes.Values['var']+'"');
      if varname.Term is TXQTermConstant then begin
        temp := varname.evaluate(xqvalue('')).toString;
        if strContains(temp, '.') then temp := strBefore(temp, '.');
        TXQueryEngineBreaker(parser.QueryEngine).addAWeirdGlobalVariable('', temp);
      end;
    end;
  end;
end;

procedure TTemplateElement.freeCaches;
var
 i: Integer;
begin
  for i:=0 to high(textRegexs) do
    wregexprFree(textRegexs[i]);
  setlength(textRegexs, 0);
  FreeAndNil(data);
  {FreeAndNil(test);
  FreeAndNil(condition);
  FreeAndNil(source);
  FreeAndNil(valuepxp);
  FreeAndNil(min);
  FreeAndNil(max);}
end;

destructor TTemplateElement.destroy;
begin
  FreeAndNil(templateAttributes);
  freeCaches;
  inherited destroy;
end;

function THtmlTemplateParser.getHTMLTree: TTreeNode;
begin
  if FHtmlTree = nil then exit(nil);
  result := FHtmlTree;
end;

function THtmlTemplateParser.getTemplateTree: TTreeNode;
begin
  if FTemplate = nil then exit(nil);
  result := FTemplate.getLastTree;
end;

function THtmlTemplateParser.parseQuery(const expression: string): IXQuery;
var
  context: TXQStaticContext;
begin
  if expression = '' then raise ETemplateParseException.Create('no expression given');
  context := nil;
  if FSingleQueryModule then context := FQueryContext.StaticContext;
  //carefully here. parseXQuery3 will keep lastquery in the engine with this query. But assigned to which context? no idea.
  //likely the outer expression is the primary context (unshared one),
  //  i.e. if the pattern is used as pattern matching its context is shared with the xquery expression around it,
  //        and if xquery is included in a pattern, the xquery shares the context of the pattern
  // but if we mix it, lastquery will still contain a reference to a static context that has expired. Which will crash when the query is freed (i.e. a new query is read)
  //using parseTerm avoids all that, since it does not keep a lastquery
  result := TXQueryEngineBreaker(FQueryEngine).parseTerm(expression, xqpmXQuery3, context);
end;

function THtmlTemplateParser.GetTemplateNamespace: TNamespaceList;
begin
  result := FTemplate.globalNamespaces;
end;

function TXQTerm_VisitorFindWeirdGlobalVariableDeclarations.visit(t: PXQTerm): TXQTerm_VisitAction;

  function arrayContains(t: TXQTermVariable): boolean;
  var
    i: Integer;
  begin
    for i:=0 to high(vars) do
      if vars[i] = t then exit(true);
    for i:=0 to high(vars) do
      if (vars[i].value = t.value) and (equalNamespaces(vars[i].namespace, t.namespace)) then
        exit(true);
    result := false;
  end;

var
  parentIsModule: boolean;
begin
  if (t^ is TXQTermDefineVariable) then begin
    parentIsModule := parent is TXQTermModule;
    if (not parentIsModule and (not (parent is TXQTermDefineFunction) or  (t^ = TXQTermDefineFunction(parent).children[high(TXQTermDefineFunction(parent).children)]) ))
       or (parentIsModule and (t^ = TXQTermModule(parent).children[high(TXQTermModule(parent).children)])) then begin
      hasVars := true;
      if listVars and (not arrayContains(TXQTermVariable(TXQTermDefineVariable(t^).getVariable))) then begin
        SetLength(vars, length(vars) + 1);
        vars[high(vars)] := TXQTermVariable(TXQTermDefineVariable(t^).getVariable);
      end;
    end;
  end else if (t^ is TXQTermPatternMatcher) and not findNestedVariables then exit(xqtvaNoRecursion);
  Result:=xqtvaContinue;
end;


function THtmlTemplateParser.GetTemplateHasRealVariableDefinitions: boolean;
var
  cur: TTemplateElement;
  visitor: TXQTerm_VisitorFindWeirdGlobalVariableDeclarations;
  temp: TXQTerm;
begin
  result := false;
  visitor := TXQTerm_VisitorFindWeirdGlobalVariableDeclarations.Create;
  try
    cur := TTemplateElement(FTemplate.getLastTree.next);
    while cur <> nil do begin
      if cur.source <> nil then begin
        temp := cur.source.Term;
        visitor.simpleTermVisit(@temp, nil);
        if visitor.hasVars then exit(true);
      end;
      cur := cur.templateNext;
    end;
  finally
    visitor.free;
  end;
end;

procedure THtmlTemplateParser.GetTemplateRealVariableDefinitions(var vars: TXQTermVariableArray; out hasDefaultVariable: Boolean);
var
  cur: TTemplateElement;
  visitor: TXQTerm_VisitorFindWeirdGlobalVariableDeclarations;
  temp: TXQTerm;
begin
  hasDefaultVariable := false;
  visitor := TXQTerm_VisitorFindWeirdGlobalVariableDeclarations.Create;
  visitor.listVars := true;
  try
    cur := TTemplateElement(FTemplate.getLastTree.next);
    while cur <> nil do begin
      if cur.source <> nil then begin
        visitor.hasVars := false;
        temp := cur.source.Term;
        visitor.simpleTermVisit(@temp, nil);
        if (cur.templateType = tetCommandShortRead) and not visitor.hasVars then
          hasDefaultVariable := true;
      end;
      cur := cur.templateNext;
    end;
  finally
    vars := visitor.vars;
    visitor.free;
  end;
end;

function THtmlTemplateParser.GetTemplateContextDependencies: TXQContextDependencies;
var
  cur: TTemplateElement;
begin
  //perhaps better to replace this with the visitor?
  result := [];
  cur := TTemplateElement(FTemplate.getLastTree.next);
  while cur <> nil do begin
    if cur.source <> nil then result += cur.source.getTerm.getContextDependencies;
    if cur.test <> nil then result += cur.test.getTerm.getContextDependencies;
    if cur.condition <> nil then result += cur.condition.getTerm.getContextDependencies;
    if cur.valuepxp <> nil then result += cur.valuepxp.getTerm.getContextDependencies;
    if cur.min <> nil then result += cur.min.getTerm.getContextDependencies;
    if cur.max <> nil then result += cur.max.getTerm.getContextDependencies;
    if cur.varname <> nil then result += cur.varname.getTerm.getContextDependencies;
    if cur.ignoreSelfTest <> nil then result += cur.ignoreSelfTest.getTerm.getContextDependencies;
    cur := cur.templateNext;
  end;
end;

{procedure THtmlTemplateParser.defineXQVariable(sender: TObject; const variable: string; const value: IXQValue);
var
  base: string;
  varname: string;
  temp: IXQValue;
begin
  if not FVariableLog.splitName(variable,base,varname) or not FVariableLog.allowPropertyDotNotation then begin
    FVariableLog.defineVariable(sender, variable, value);
    exit;
  end;
  if FVariableLog.hasVariable(base, nil) or not FOldVariableLog.hasVariable(base, nil) then begin
    FVariableLog.defineVariable(sender, variable, value);
    exit;
  end;
  temp := FOldVariableLog.get(base);
  if not (temp is TXQValueObject) then raise EXQEvaluationException.create('pxp:OBJECT', 'Set object property, but variable is no object');
  FVariableLog.defineVariable(sender, base, (temp as TXQValueObject).setImmutable(varname, value));
end;}

function THtmlTemplateParser.GetVariableLogCondensed: TXQVariableChangeLog;
begin
  if FVariableLogCondensed = nil then FVariableLogCondensed := FVariableLog.condensed;
  result := FVariableLogCondensed;
end;

function THtmlTemplateParser.GetVariables: TXQVariableChangeLog;
begin
  if FVariables = nil then begin
    FVariables := FVariableLog.finalValues();
    FVariables.readonly := true;
  end;
  result := FVariables;
end;

{function THtmlTemplateParser.evaluateXQVariable(sender: TObject; const variable: string; var value: IXQValue): boolean;
var
  temp: TXQValue;
begin
  ignore(sender);
  temp := nil;
  if not FVariableLog.hasVariableOrObject(variable, @temp) then
    if not FOldVariableLog.hasVariableOrObject(variable, @temp) then exit(false);
  if temp <> nil then value := temp
  else value := xqvalue();
  result := true;
end;}

type TXQCollationRawStrBoolFunction = function (const a,b: RawByteString): boolean;
type TXQCollationStrBoolFunction = function (const a,b: string): boolean;

function THtmlTemplateParser.templateElementFitHTMLOpen(html: TTreeNode;
  template: TTemplateElement): Boolean;
var
  name, strategy: string;
  j, k, strategyi: Integer;
  templateList: TStringArray;
  htmlList: TStringArray;
  found: Boolean;
  attrib: TTreeAttribute;
  tempContext: TXQEvaluationContext;
  regexp: TWrappedRegExpr;
  caseSensitive: Boolean;

  function fir(cs, csi: TXQCollationRawStrBoolFunction): TXQCollationRawStrBoolFunction;inline; overload;
  begin
    if caseSensitive then result := cs
    else result := csi;
  end;
  function fis(cs, csi: TXQCollationStrBoolFunction): TXQCollationStrBoolFunction;inline; overload;
  begin
    if caseSensitive then result := cs
    else result := csi;
  end;

begin
  if html.typ <> tetOpen then exit(false);
  case template.templateType of
    tetHTMLOpen: if (html.hash <> template.hash) or not striequal(html.value, template.value) then
      exit(false);
    tetMatchElementOpen: ;
    else exit(false);
  end;
  if (template.attributes = nil) and (template.templateAttributes = nil) then
    exit(true);
  for attrib in template.attributes do begin
    name := attrib.value;
    if html.attributes = nil then exit(false);
    caseSensitive := FAttributeDefaultCaseSensitive;
    strategy := FAttributeDefaultMatching;
    strategyi := FAttributeMatching.IndexOfName(attrib.value);
    if strategyi <> -1 then begin
      if FAttributeMatching.ValueFromIndex[strategyi] <> '' then strategy := FAttributeMatching.ValueFromIndex[strategyi];
      if FAttributeMatching.Objects[strategyi] <> nil then caseSensitive := FAttributeMatching.Objects[strategyi] <> nil;
    end;
    case strategy of
      'eq', 'is' {is is deprecated}:
        if not fir(@strEqual, @striEqual)(html.getAttribute(name), attrib.realvalue) then
          exit(false);
      'list-contains': begin
        templateList := strSplit(attrib.realvalue, ' ', false);
        htmlList := strSplit(html.getAttribute(name), ' ', false);
        for j:=0 to high(templateList) do begin
          found := false;
          for k:= 0 to high(htmlList) do if fir(@strEqual, @striEqual)(templateList[j], htmlList[k]) then begin found := true; break; end;
          if not found then exit(false);
        end;
      end;
      'starts-with': if not fis(@strBeginsWith, @striBeginsWith)(html.getAttribute(name), attrib.realvalue) then exit(false);
      'ends-with':   if not fis(@strEndsWith, @striEndsWith)(html.getAttribute(name), attrib.realvalue) then exit(false);
      'contains':    if not fis(@strContains, @striContains)(html.getAttribute(name), attrib.realvalue) then exit(false);
      'matches':     begin
        if caseSensitive then regexp:=wregexprParse(attrib.realvalue, [wrfSingleLine])
        else regexp:=wregexprParse(attrib.realvalue,  [wrfIgnoreCase,wrfSingleLine]);
        try
          if not wregexprMatches(regexp, html.getAttribute(name)) then exit(false);
        finally
          wregexprFree(regexp);
        end;
      end;
      else raise EHTMLParseMatchingException.Create('Invalid attribute matching kind', self);
    end;
  end;
  if template.templateAttributes = nil then exit(true);
  if template.condition = nil then exit(true);
  tempContext := FQueryContext;
  tempContext.ParentElement := html;
  tempContext.TextNode := nil;
  result := template.condition.evaluate(tempContext).toBoolean;
end;

procedure THtmlTemplateParser.resetAttributeMatching;
var
  el: TTemplateElement;
  i: Integer;
  temps: String;
begin
  FAttributeDefaultMatching := 'eq';
  FAttributeDefaultCaseSensitive := false;
  FAttributeMatching.Clear;
  FAttributeMatching.Values['class'] := 'list-contains';


  el := TTemplateElement(FTemplate.getLastTree.next);
  while el <> nil do begin
    if (el.templateType = tetCommandMeta) and (el.templateAttributes<>nil) then begin
      if el.templateAttributes.IndexOfName('attribute-matching') >= 0 then
        FAttributeDefaultMatching := el.templateAttributes.Values['attribute-matching'];
      //if el.templateAttributes.IndexOfName('attribute-collation') >= 0 then
      //  FAttributeDefaultCollation := el.templateAttributes.Values['attribute-collation'];
      if el.templateAttributes.IndexOfName('attribute-case-sensitive') >= 0 then
        case el.templateAttributes.Values['attribute-case-sensitive'] of
          '':;//ignore
          'false', 'case-insensitive', 'insensitive': FAttributeDefaultCaseSensitive := false;
          else FAttributeDefaultCaseSensitive := true;
        end;

    end else if (el.templateType = tetCommandMetaAttribute) and (el.templateAttributes<>nil) then begin
      if el.templateAttributes.Values['name'] = '' then
        raise EHTMLParseException.Create('Need attribute name on meta-attribute element');
      i := FAttributeMatching.IndexOfName(el.templateAttributes.Values['name']);
      temps := el.templateAttributes.Values['matching'];
      if i >= 0 then begin
        FAttributeMatching.ValueFromIndex[i] := temps;
        if temps = '' then i := -1; //fpc, lol
      end;
      if i < 0 then begin
        FAttributeMatching.Add(el.templateAttributes.Values['name'] + '=' + temps);
        i := FAttributeMatching.count - 1;
      end;
      case el.templateAttributes.Values['case-sensitive'] of
        '':;//ignore
        'false', 'case-insensitive', 'insensitive': FAttributeMatching.Objects[i] := nil;
        else FAttributeMatching.Objects[i] := Tobject(-1);
      end;
    end;
    el := el.templateNext;
  end;
end;

function THtmlTemplateParser.matchTemplateTree(htmlParent, htmlStart, htmlEnd: TTreeNode; templateStart, templateEnd: TTemplateElement
  ): boolean;

var xpathText: TTreeNode;

  function performPXPEvaluation(const pxp: IXQuery): IXQValue;
  var
    tempContext: TXQEvaluationContext;
  begin
    if pxp = nil then exit(xqvalue());
    tempContext := FQueryContext;
    tempContext.ParentElement := htmlParent;
    tempContext.TextNode := xpathText;
    result := pxp.evaluate(tempContext);
  end;

  procedure HandleMatchText;
  var
   i: Integer;
   ok: Boolean;
  begin
    //if we find a text match we can assume it is a true match
    if htmlStart.typ = tetText then begin
      ok := true;
      for i := 0 to high(templateStart.textRegexs) do
        if not wregexprMatches(templateStart.textRegexs[i], htmlStart.value) then begin
          ok := false;
          break;
        end;
      if ok and (templateStart.condition <> nil) then
        ok := performPXPEvaluation(templateStart.condition).toBoolean;
      if ok then begin
        templateStart.match := htmlStart;
        templateStart := templateStart.templateNext;
      end;
    end;
    htmlStart := htmlStart.next;
  end;

  procedure HandleOptional;
  var ok: boolean;
  begin
    //If an element is option it can either be there (preferred) or not. Therefore we simple try both cases
    //Notice that this modifies the template, and it is NOT THREAD SAFE (so don't share
    //one instance, you can of course still use instances in different threads)
    Exclude(templateStart.flags, tefOptional);
    ok := matchTemplateTree(htmlParent, htmlStart, htmlEnd, templateStart, templateEnd);
    Include(templateStart.flags, tefOptional);
    if ok then templateStart := templateEnd
    else templateStart := templateStart.templateReverse.templateNext;
  end;

  function HandleMatchOpen: boolean;
  begin
    //To check if a node matches a template node we have to check all children, if they don't match
    //we have to test it with another node
    //But once a element E match we can assume that there is no better match on the same level (e.g. a
    //match F with E.parent = F.parent), because this is simple list matching
    result := false;
    if (not templateElementFitHTMLOpen(htmlStart, templateStart)) then htmlStart:=htmlStart.next
    else begin
      templateStart.match := htmlStart;
      if (not matchTemplateTree(htmlStart, htmlStart.next, htmlStart.reverse, templateStart.templateNext, templateStart.templateReverse)) then htmlStart:=htmlStart.next
      else begin
        htmlStart := htmlStart.reverse.next;
        templateStart := templateStart.templateReverse.templateNext;
        result := true;
      end;
    end;
  end;

  procedure HandleCommandRead;
  var
   value:IXQValue;
   oldvarcount: Integer;
   attribs: tStringAttributeList;
   regex: String;
   name: String;
   props: TStringArray;
   tempa: TXQVArray;
  begin
    attribs := templateStart.templateAttributes;

    oldvarcount := FVariableLog.count;
    value:=performPXPEvaluation(templateStart.source);

    regex := attribs.Values['regex'];
    if regex<>'' then begin
      tempa := xqvalueArray([value, xqvalue(regex), xqvalue(StrToIntDef(templateStart.templateAttributes.Values['submatch'],0)), xqvalue('i')]);
      value := xqFunctionExtract(length(tempa), @tempa[0]);
    end;



    if templateStart.varname <> nil then begin
      name := Trim(performPXPEvaluation(templateStart.varname).toString);
      props := strSplit(name, '.');
      if length(props) > 0 then name := arrayDelete(props, 0);
      FVariableLog.addObjectModification(name, value, '', props);
    end else if (FUnnamedVariableName <> '') and (oldvarcount = FVariableLog.count) then
      FVariableLog.add(FUnnamedVariableName, value);

    templateStart := templateStart.templateReverse;
  end;

  procedure HandleCommandShortRead;
  var varcount: integer;
    read: IXQValue;
  begin
    varcount:=FVariableLog.count;
    read := performPXPEvaluation(templateStart.source);
    if (FUnnamedVariableName <> '') and (varcount = FVariableLog.count) then
      FVariableLog.add(FUnnamedVariableName, read);
    templateStart := templateStart.templateReverse;
  end;

  function HandleCommandPseudoIf: boolean;
  var
    trueif, satisfied: Boolean;
  begin
    trueif := templateStart.templateType in [tetCommandIfOpen,tetCommandElseOpen];
    satisfied:=(templateStart.test = nil) or  performPXPEvaluation(templateStart.test).toBoolean;

    if satisfied then
      templateStart.match := htmlStart
    else begin
       templateStart := templateStart.templateReverse; //skip block
       assert(templateStart.typ = tetClose);
       templateStart := templateStart.templateNext;    //skip block end
       if trueif then begin
         if (templateStart.templateType = tetCommandElseOpen) then
           if HandleCommandPseudoIf() then
             templateStart := templateStart.templateNext; //enter else, if "if" is not satisfied, but "else" is satisfied
           //else skip else block
       end;
     end;
     result := satisfied;
  end;



  procedure SkipFollowingElses;
  begin
    templateStart := templateStart.templateNext;
    while templateStart.templateType = tetCommandElseOpen do begin
      templateStart := templateStart.templateReverse;
      assert(templateStart.templateType = tetCommandElseClose);
      templateStart := templateStart.templateNext;
    end;
  end;

  procedure HandleCommandLoopOpen;
  begin
    //Two possible cases:
    //1. Continued in loop (preferred of course)
    //2. Jumped over loop
    templateStart.contentRepetitions+=1;
    if ((templateStart.max = nil) or (templateStart.contentRepetitions <= performPXPEvaluation(templateStart.max).toInt64))
       and matchTemplateTree(htmlParent, htmlStart, htmlEnd, templateStart.templateNext, templateEnd) then begin
      templateStart.contentRepetitions-=1;
      templateStart := templateEnd;
    end else begin
      templateStart.contentRepetitions-=1;
      if (templateStart.min = nil) or (performPXPEvaluation(templateStart.min).toInt64 <= templateStart.contentRepetitions) then templateStart := templateStart.templateReverse.templateNext
      else htmlStart := htmlStart.next;
    end;
  end;


  var realHtmlStart: TTreeNode;
    procedure HandleCommandLoopClose;
    begin
      //Jump to loop start if a html element was read in the loop
      //The condition is necessary, because if the loop is executed without
      //reading a html element, it can be executed again, and again, and ... =>
      //endless loop
      if realHtmlStart <> htmlStart then
        templateStart := templateStart.templateReverse //jump to loop start (will then call HandleCommandLoopOpen?)
       else
        templateStart := templateStart.templateNext
    end;


  var switchCommandAccepted: boolean;

  procedure HandleCommandSwitch(prioritized: boolean);
  var curChild: TTemplateElement;

    procedure switchTemplateCommand;
    var value: IXQValue;
      function elementFit(e: TTemplateElement): boolean;
      var evaluatedvalue: IXQValue;
      begin
        if (e.templateAttributes = nil) or (e.templateAttributes.Count = 0) then exit(true);
        result := (e.test = nil) or performPXPEvaluation(e.test).toBoolean;
        if not result then exit;
        if e.valuepxp = nil then exit;
        evaluatedvalue := performPXPEvaluation(e.valuepxp);
        result := FQueryEngine.StaticContext.compareGeneral(evaluatedvalue, value, nil, 0);
      end;

    begin
      if templateStart.valuepxp <> nil then value := performPXPEvaluation(templateStart.valuepxp)
      else value := xqvalue();

      while curChild <> nil do begin //enumerate all child tags
        if curChild.templateType in [tetHTMLOpen,tetHTMLClose,tetMatchElementOpen,tetMatchElementClose] then raise ETemplateParseException.Create('A switch command must consist entirely of only template commands or only html tags');
        if curChild.templateType = tetCommandSwitchOpen then raise ETemplateParseException.Create('A switch command may not be a direct child of another switch command');
        if elementFit(curChild) then begin
          templateStart := curChild;
          switchCommandAccepted:=true;
          exit;
        end else curChild := TTemplateElement(curChild.getNextSibling());
      end;

      templateStart:=templateStart.templateReverse;
    end;

    procedure switchHTML;
    begin
      //TODO: understand and document how this all works

      //idea for switch (html): foreach html position (<- that loop is in the caller's caller): foreach template child: check if match

      while curChild <> nil do begin //enumerate all child tags
        if tefOptional in curChild.flags then raise ETemplateParseException.Create('A direct child of the template:switch construct may not have the attribute template:optional (it is optional anyways)');
        if curChild.templateType >= firstRealTemplateType then raise ETemplateParseException.Create('A switch command must consist entirely of only template commands or only html tags');
        if templateElementFitHTMLOpen(htmlStart, curChild) and
            matchTemplateTree(htmlStart, htmlStart.next, htmlStart.reverse, curChild.templateNext, curChild.templateReverse) then begin
          //found match
          htmlStart := htmlStart.reverse.next;
          templateStart := templateStart.templateReverse.templateNext;
          exit;
        end;
        //no match, try other matches
        curChild := TTemplateElement(curChild.getNextSibling());
      end;

      htmlStart:=htmlStart.next; //no match
    end;

    procedure switchPrioritized;
    var oldHtmlStart: TTreeNode;
    begin
      //TODO: understand and document how this all works

      //idea for switch-prioritized: foreach template child: foreach html position: check if match

      oldHtmlStart := htmlStart;
      while curChild <> nil do begin //enumerate all child tags
        if tefOptional in curChild.flags then raise ETemplateParseException.Create('A direct child of the template:switch-prioritized construct may not have the attribute template:optional (it is optional anyways)');
        if curChild.templateType >= firstRealTemplateType then raise ETemplateParseException.Create('A switch-prioritized command must consist entirely of only html tags');

        htmlStart := oldHtmlStart;
        while (htmlStart <> nil) and ((htmlStart <> htmlEnd.next)) do begin
          if templateElementFitHTMLOpen(htmlStart, curChild) and
            matchTemplateTree(htmlStart, htmlStart.next, htmlStart.reverse, curChild.templateNext, curChild.templateReverse) then begin
            //found match
            htmlStart := htmlStart.reverse.next;
            templateStart := templateStart.templateReverse.templateNext;
            exit;
          end;
          htmlStart := htmlStart.next;
        end;


        //no match, try other matches
        curChild := TTemplateElement(curChild.getNextSibling());
      end;

      htmlStart:=htmlEnd.next; //no match possible
    end;

  begin
    templateStart.match := htmlStart;
    curChild:=TTemplateElement(templateStart.getFirstChild());
    if curChild = nil then  begin
      templateStart:=templateStart.templateReverse;
      exit;
    end;

    if prioritized then switchPrioritized
    else if curChild.templateType >= firstRealTemplateType then switchTemplateCommand
    else switchHTML;
  end;

  procedure HandleCommandSiblingsHeader;
  var outSiblingIndices: array of integer;
    childrenStart, lastHTMLStartMatch: TTreeNode;
    header, curChild: TTemplateElement;
    counts: array of integer;
    minCounts, maxCounts, usedCounts: pinteger;
    count: Integer;
    remainingMinCount, firstActiveChild, i: integer;
    data: TCommandSiblingHeaderData;
    //found: Boolean;
  begin
    //all element children of t:siblings-header are numbered 0..k and the children of t:siblings are numbered the same.
    //HandleCommandSiblingsHeader records the id-numbers of the children in the order they occur in the HTML.
    //HandleCommandSiblings then matches the children of t:siblings in that order.

    //As all matching it allows additional nodes between the matched siblings, but unlike the rest it only checks for siblings,
    //i.e. children of htmlStart and not for descendants;

    header := templateStart;
    data := TCommandSiblingHeaderData(header.data);
    childrenStart := header.getFirstChild();
    if childrenStart = nil then raise ETemplateParseException.Create('<t:siblings> without children');

    count := length(data.children);
    SetLength(counts, 3 * count);
    minCounts := @counts[0];
    maxCounts := @counts[count];
    usedCounts := @counts[2*count];

    remainingMinCount := 0;

    for i := 0 to count - 1 do begin
      if ((data.children[i].test <> nil) and not performPXPEvaluation(data.children[i].test).toBoolean)
        or ((data.countChildren[i] <> nil) and (data.countChildren[i].test <> nil) and not performPXPEvaluation(data.countChildren[i].test).toBoolean)  then begin
        minCounts[i] := 0;
        maxCounts[i] := 0;
      end else if data.countChildren[i] = nil then begin
        if tefOptional in data.children[i].flags then minCounts[i] := 0
        else minCounts[i] := 1;
        maxCounts[i] := 1;
      end else begin
        if data.countChildren[i].min = nil then minCounts[i] := 0
        else minCounts[i] := performPXPEvaluation(data.countChildren[i].min).toInt64;
        if data.countChildren[i].max = nil then maxCounts[i] := -1
        else maxCounts[i] := performPXPEvaluation(data.countChildren[i].max).toInt64;
      end;
      remainingMinCount += minCounts[i];
    end;

    outSiblingIndices := nil;

    firstActiveChild := 0;

    lastHTMLStartMatch := htmlStart;

    while (htmlStart <> htmlEnd.next) and (htmlStart <> nil) do begin
      //found := false;
      for i := firstActiveChild to count - 1 do begin
        if usedCounts[i] = maxCounts[i] then continue;
        curChild := data.children[i];;
        if templateElementFitHTMLOpen(htmlStart, curChild) then begin
          curChild.match := htmlStart;
          if  matchTemplateTree(htmlStart, htmlStart.next, htmlStart.reverse, curChild.templateNext, curChild.templateReverse) then begin
            lastHTMLStartMatch := htmlStart.getNextSibling();
            SetLength(outSiblingIndices, length(outSiblingIndices) + 1);
            outSiblingIndices[high(outSiblingIndices)] := i;
            if usedCounts[i] < minCounts[i] then dec(remainingMinCount);
            inc(usedCounts[i]);
            if (i = firstActiveChild) and (usedCounts[i] = maxCounts[i]) then inc(firstActiveChild);
            //found := true;
            break;
          end;
        end;
      end;
      //uncommented to not allow nodes between the matched siblings
      //if not found then break;
      htmlStart := htmlStart.getNextSibling();
    end;

    if remainingMinCount <= 0 then begin
      templateStart := templateStart.templateReverse.templateNext; //accept
      FSiblingMatching[data.id].order := outSiblingIndices;
      htmlStart := lastHTMLStartMatch;
    end else htmlStart := htmlEnd.next; //reject
  end;

  procedure HandleCommandSiblings;
  var
    i: integer;
    data: TCommandSiblingData;
    found: Boolean;
    siblings: TTemplateElement;
    tempHTMLStart: TTreeNode;
  begin
    siblings := templateStart;
    data := TCommandSiblingData(siblings.data);

    found := true;
    with FSiblingMatching[data.id] do begin
      for i := 0 to high(order) do begin
        templateStart := data.children[order[i]];
        found := false;
        while (htmlStart <> nil) and
              ((htmlStart <> htmlEnd.next)) do begin
          tempHTMLStart := htmlStart;
          if HandleMatchOpen then begin //this moves to the next descendant, but we want the next sibling
            found := true;
            break;
          end;
          htmlStart := tempHtmlStart.getNextSibling();
        end;
      end;
    end;
    if not found then begin htmlStart := htmlEnd.next; templateStart := siblings;  exit; end;

    templateStart := siblings.templateReverse.templateNext; //accept
  end;

var level: integer;
begin
  if htmlStart = nil then exit(false);
  if templateStart = nil then exit(false);

  realHtmlStart := htmlStart;
 // assert(templateStart <> templateEnd);
  level := FVariableLog.pushAll;
  xpathText := nil;
  switchCommandAccepted:=false;
  while (htmlStart <> nil) and
        (templateStart <> nil) and (templateStart <> templateEnd) and
        ((htmlStart <> htmlEnd.next)) do begin
            if htmlStart.typ = tetText then xpathText := htmlStart;
            if (templateStart.ignoreSelfTest <> nil) and performPXPEvaluation(templateStart.ignoreSelfTest).toBooleanEffective then begin
              templateStart := templateStart.templateNext;
              continue;
            end;
            if not switchCommandAccepted and (templateStart.templateType <> tetIgnore) and
                (templateStart.test <> nil) then
              if not HandleCommandPseudoIf then continue;
            if tefSwitchChild in templateStart.flags then begin
              if switchCommandAccepted then switchCommandAccepted:=false
              else begin //try other switch children (?)
                if templateStart.typ in TreeNodesWithChildren then templateStart := templateStart.templateReverse.templateNext
                else templateStart := templateStart.templateNext;
                continue;
              end;
            end;
            if tefOptional in templateStart.flags then begin
              HandleOptional;
              continue;
            end;
            case templateStart.templateType of
              tetMatchText: HandleMatchText;
              tetHTMLText: raise ETemplateParseException.Create('Assertion fail: Template text has been converted to text-match');
              tetHTMLOpen, tetMatchElementOpen: HandleMatchOpen;
              tetHTMLClose, tetMatchElementClose:
                if templateStart.templateReverse.ignoreSelfTest <> nil then templateStart := templateStart.templateNext //there is no way to get the value of the query now, is there?
                else raise ETemplateParseException.Create('Assertion fail: Closing template tag </'+templateStart.value+'> not matched');

              tetCommandRead: HandleCommandRead;
              tetCommandShortRead: HandleCommandShortRead;

              tetCommandLoopOpen: HandleCommandLoopOpen;
              tetCommandLoopClose: HandleCommandLoopClose;

              tetCommandSwitchOpen: HandleCommandSwitch(false);
              tetCommandSwitchPrioritizedOpen: HandleCommandSwitch(true);

              tetCommandSiblingsOpen: HandleCommandSiblings;
              tetCommandSiblingsHeaderOpen: HandleCommandSiblingsHeader;

              tetIgnore, tetCommandMeta, tetCommandMetaAttribute, tetCommandIfOpen, tetCommandSwitchClose, tetCommandSiblingsHeaderClose, tetCommandSiblingsClose : templateStart := templateStart.templateNext;

              tetCommandIfClose, tetCommandElseClose: SkipFollowingElses;

              tetCommandElseOpen: raise ETemplateParseException.Create('Found <else> tag without previous <if>');

              else raise ETemplateParseException.Create('Unknown template element type - internal error');
            end
        end;

  result := templateStart = templateEnd;
  if not result then
    FVariableLog.popAll(level);
end;

procedure THtmlTemplateParser.parseHTMLSimple(html, uri, contenttype: string);
begin
  FHTML.trimText := FTrimTextNodes = ttnWhenLoading;
  FHTML.TargetEncoding := OutputEncoding;
  FHtmlTree := FHTML.parseTree(html, (uri), contenttype);

  FQueryContext.RootElement := FHtmlTree;
  if FHtmlTree = nil then exit;
  if FHtmlTree.document is TTreeDocument then
    FQueryEngine.StaticContext.baseURI := FHtmlTree.getDocument().baseURI; //todo: what was this for?

  if FTrimTextNodes = ttnWhenLoadingEmptyOnly then
    FHTML.removeEmptyTextNodes(true);
end;

procedure THtmlTemplateParser.initializeCaches;
var
  cur: TTemplateElement;
begin
  FSiblingMatching := nil;
  if FTemplate.getLastTree <> nil then begin
    if (FTemplate.getLastTree.getEncoding <> OutputEncoding) then begin
      cur := TTemplateElement(FTemplate.getLastTree.next);
      while cur <> nil do begin
        if (cur.templateAttributes<>nil) then
          cur.templateAttributes.Text := strConvert(cur.templateAttributes.Text, ftemplate.getLastTree.getEncoding, OutputEncoding);
        if (cur.templateAttributes<>nil) or (cur.templateType in [tetCommandShortRead, tetCommandSiblingsHeaderOpen, tetCommandSiblingsOpen]) then
          cur.initializeCaches(self,true);
        cur := cur.templateNext;
      end;
    end else begin
      cur := TTemplateElement(FTemplate.getLastTree.next);
      while cur <> nil do begin
        if (cur.templateAttributes<>nil) or (cur.templateType in [tetCommandShortRead, tetCommandSiblingsHeaderOpen, tetCommandSiblingsOpen]) then
          cur.initializeCaches(self,lastTrimTextNodes <> FTrimTextNodes);
        cur := cur.templateNext;
      end;
    end;
  end;
end;

function THtmlTemplateParser.matchLastTrees: Boolean;
var cur,last,realLast:TTemplateElement;
    temp: TTreeNode;
    err: String;
    i: Integer;
    oldFunctionCount: Integer;
begin
  FreeAndNil(FVariables);
  if FKeepOldVariables = kpvForget then
    FVariableLog.Clear
  else begin
    //convert all node variables to string (because the nodes point to a tree which we will destroy soon)
    FVariableLog.stringifyNodes;
    if FKeepOldVariables = kpvKeepValues then
      FOldVariableLog.takeFrom(FVariableLog);;
  end;
  FreeAndNil(FVariableLogCondensed);

  oldFunctionCount := length(FQueryEngine.StaticContext.functions);

  initializeCaches;
  FTemplate.getLastTree.setEncoding(outputEncoding,true,false); //todo: check this for &amp; in templates!
  lastTrimTextNodes := FTrimTextNodes;


  if FParsingExceptions then begin
    cur := TTemplateElement(FTemplate.getLastTree.next);
    while cur <> nil do begin
      cur.match := nil;
      cur := cur.templateNext;
    end;
  end;

  temp := FHtmlTree;
  if temp is TTreeDocument then temp := temp.next;
  if temp = nil then raise EHTMLParseMatchingException.create('No HTML tree', self);
  if FTemplate.getLastTree = nil then raise EHTMLParseMatchingException.create('No template tree', self);
  result:=matchTemplateTree(FHtmlTree, temp, FHtmlTree.reverse, TTemplateElement(FTemplate.getLastTree.next), TTemplateElement(FTemplate.getLastTree.reverse));

  //delete functions, so multiple parsing attempts do not intermix
  for i := oldFunctionCount to high(FQueryEngine.StaticContext.functions) do
    FQueryEngine.StaticContext.functions[i].free;
  SetLength(FQueryEngine.StaticContext.functions, oldFunctionCount);

  if not result and FParsingExceptions then begin
    cur := TTemplateElement(FTemplate.getLastTree.next);
    if cur = nil then raise EHTMLParseException.Create('No template');
    cur := cur.templateNext;
    realLast := nil;
    last := nil;
    while cur <> nil do begin
      case cur.templateType of
        tetHTMLOpen, tetHTMLText, tetMatchElementOpen: begin
          if (cur.match = nil) then begin
            err := 'Matching of template '+ftemplate.getLastTree.baseURI+' failed.'#13#10+
                   'Couldn''t find a match for: '+cur.toString+#13#10;
            if realLast <> nil then err += 'Previous element is:'+reallast.toString+#13#10;
            if last <> nil then err += 'Last match was:'+last.toString+' with '+TTemplateElement(last).match.toString;
            raise EHTMLParseMatchingException.create(err, self);
          end;
          last:=cur;
        end;
        tetCommandIfOpen: begin
          if cur.match = nil then cur := cur.templateReverse;
          last:=cur;
        end;
      end;

      realLast := cur;
      cur := cur.templateNext;
    end;
    raise EHTMLParseMatchingException.create('Matching of template '+FTemplate.getLastTree.baseURI+' failed. for an unknown reason', self);
  end;
//TODODO  for i:=1 to variableLogStart do FVariableLog.Delete(0); //remove the old variables from the changelog
end;

constructor THtmlTemplateParser.create;
begin
  FOldVariableLog := TXQVariableChangeLog.create;
  FTemplate := TTreeParser.Create;
  FTemplate.parsingModel:=pmStrict;
  FTemplate.treeNodeClass:=TTemplateElement;
  FTemplate.globalNamespaces.Add(TNamespace.make(HTMLPARSER_NAMESPACE_URL, 'template'));
  FTemplate.globalNamespaces.Add(TNamespace.make(HTMLPARSER_NAMESPACE_URL, 't'));
  FTemplate.trimText:=true;
  FHTML := TTreeParser.Create;
  FHTML.parsingModel:=pmHTML;
  FHTML.readComments:=true;
  outputEncoding:=CP_UTF8;
  FParsingExceptions := true;
  FKeepOldVariables:=kpvForget;
  FRepetitionRegEx:=wregexprParse('^ *[{] *([0-9]+) *(, *([0-9]+) *)?[}] *', [wrfSingleLine]);
  FUnnamedVariableName:='_result';
  FVeryShortNotation:=true;
  FTrimTextNodes:=ttnForMatching;
  FSingleQueryModule := true;

  FAttributeMatching := TStringList.Create;

  FQueryEngine := TXQueryEngine.create;
  FQueryEngine.ParsingOptions.AllowPropertyDotNotation:=xqpdnAllowFullDotNotation;
  FQueryEngine.ParsingOptions.StringEntities:=xqseIgnoreLikeXPath;
  //FQueryEngine.OnDefineVariable:=@defineXQVariable;
  //FQueryEngine.OnEvaluateVariable:=@evaluateXQVariable;
  FQueryEngine.globalNamespaces.Add(TNamespace.make(HTMLPARSER_NAMESPACE_URL, 'template'));
  FQueryEngine.globalNamespaces.Add(TNamespace.make(HTMLPARSER_NAMESPACE_URL, 't'));
  FQueryContext := FQueryEngine.getEvaluationContext(FQueryEngine.StaticContext);

  FVariableLog := FQueryEngine.VariableChangelog;
  FVariableLog.parentLog := FOldVariableLog;
end;

destructor THtmlTemplateParser.destroy;
begin
  FTemplate.Free;
  FHTML.Free;
  FQueryEngine.Free;
  FAttributeMatching.Free;
  wregexprFree(FRepetitionRegEx);
  FreeAndNil(FVariables);
  FVariableLogCondensed.free;
  FOldVariableLog.Free;
  inherited destroy;
end;

function THtmlTemplateParser.parseHTML(html: string; htmlFileName: string; contentType: string): boolean;
begin
  parseHTMLSimple(html, htmlFileName, contentType);
  Result := matchLastTrees;
end;

function THtmlTemplateParser.parseHTMLFile(htmlfilename: string):boolean;
begin
  result:=parseHTML(strLoadFromFile(htmlfilename),htmlfilename);
end;

procedure THtmlTemplateParser.parseTemplate(template: string;
  templateName: string);
var el: TTemplateElement;
    defaultTextMatching: String;
    defaultCaseSensitive: string;
    i: Integer;
    looper: TTemplateElement;
    temp: TTemplateElement;
    matches: TWrappedMatchArray;
begin
   //read template
  FTemplate.parseTree(template, templateName);
  el := TTemplateElement(FTemplate.getLastTree.next);
  while el <> nil do begin
    el.postprocess(self);
    if (el.typ = tetOpen) and (el.templateType = tetCommandShortRead) then
      el := el.templateReverse
     else
      el := el.templateNext
  end;


  //detect meta encoding (doesn't change encoding; just sets it, so we can convert from it to another one later)
  el := TTemplateElement(FTemplate.getLastTree.next);


  defaultTextMatching := 'starts-with';
  defaultCaseSensitive := '';

  el := TTemplateElement(FTemplate.getLastTree.next);
  while el <> nil do begin
    if (el.templateType = tetCommandMeta) and (el.templateAttributes<>nil) then begin
      if el.templateAttributes.Values['encoding'] <> '' then
        raise EHTMLParseException.Create('The meta encoding attribute is deprecated');
      if el.templateAttributes.Values['text-matching'] <> '' then
        defaultTextMatching := el.templateAttributes.Values['text-matching']
      else if el.templateAttributes.Values['default-text-matching'] <> '' then {deprecated}
        defaultTextMatching := el.templateAttributes.Values['default-text-matching'];
      i := el.templateAttributes.IndexOfName('text-case-sensitive');
      if i < 0 then i := el.templateAttributes.IndexOfName('default-text-case-sensitive'); {deprecated}
      if i >= 0 then begin
        defaultCaseSensitive := el.templateAttributes.ValueFromIndex[i];
        if defaultCaseSensitive = '' then defaultCaseSensitive := 'true';
      end;
    end else if el.templateType = tetHTMLText then begin
      if (FVeryShortNotation) and (el.value <> '') then begin
        if el.value[1] = '?' then begin
          delete(el.value,1,1);
          temp := TTemplateElement(el.getPrevious());
          if temp.typ = tetClose then temp := temp.templateReverse;
          temp.flags += [tefOptional];
        end;
        if (el.value <> '') and ((el.value[1] in ['*', '+']) or ((el.value[1] = '{') and wregexprExtract(FRepetitionRegEx, el.value, matches))) then begin
          looper := TTemplateElement.create(tetCommandLoopOpen);
          TTemplateElement(el.getPrevious()).insertSurrounding(looper, TTemplateElement.create(tetCommandLoopClose));
          if el.value[1] <> '{' then begin
            if el.value[1] = '+' then looper.setTemplateAttribute('min', '1');
            delete(el.value,1,1);
          end else begin
            looper.setTemplateAttribute('min', matches[1]);
            if (length(matches) < 4) or (length(matches[3]) <= 0) then looper.setTemplateAttribute('max', matches[1])
            else looper.setTemplateAttribute('max', matches[3]);
            delete(el.value,1,length(matches[0]));
          end;
        end;
        if el.value = '' then el.templateType := tetIgnore
        else if el.value[1] = '{' then begin
          el.value[1] := ' ';
          el.value[length(el.value)] := ' ';
          el.insertSurrounding(TTemplateElement.create(tetCommandShortRead));
          el.templateType := tetIgnore;
        end;
      end;
      if el.templateType = tetHTMLText then begin
        el.templateType := tetMatchText;
        if el.templateAttributes = nil then el.templateAttributes := TStringList.Create;
        el.templateAttributes.Values[defaultTextMatching] := el.value;
      end;
    end;
    if (el.templateType = tetMatchText) and (defaultCaseSensitive <> '') then begin
      if el.templateAttributes = nil then el.templateAttributes := TStringList.Create;
      if el.templateAttributes.IndexOfName('case-sensitive') < 0 then
        el.templateAttributes.Values['case-sensitive'] := defaultCaseSensitive;
    end;
    el := el.templateNext;
  end;

  resetAttributeMatching;
end;


procedure THtmlTemplateParser.parseTemplateFile(templatefilename: string);
begin
  parseTemplate(strLoadFromFile(templatefilename),templatefilename);
end;

function THtmlTemplateParser.replaceVarsOld(s: string; customReplace: TReplaceFunction): string;
var f,i:longint;
    temp,value:string;
    tempxqvalue: IXQValue;
begin
  Result:='';
  i:=1;
  while i<=length(s) do begin
    if s[i]='$' then begin
      f:=i+1;
      while (i<=length(s)) and (s[i]<>';')  do inc(i);
      temp:=copy(s,f,i-f);
      tempxqvalue:=xqvalue();

      if pos('.', temp) = 0 then tempxqvalue := FVariableLog.get(temp)
      else begin
        tempxqvalue := FVariableLog.get(strSplitGet('.', temp));
        while (temp <> '') and (tempxqvalue is TXQValueObject) do
          tempxqvalue := tempxqvalue.getProperty(strSplitGet('.', temp));
      end;

      value:=tempxqvalue.toString;
      if assigned(customReplace) then customReplace(temp,value);
    //  OutputDebugString(pchar(parser.variables.Text));
      result+=value;
    end else Result+=s[i];
    i+=1;
  end;
end;

function THtmlTemplateParser.replaceEnclosedExpressions(str: string): string;
var
  standard: Boolean;
  i: Integer;
begin
  standard := true;
  for i:=1 to length(str) do
    if str[i] in ['{', '}' ] then begin
      standard := false;
      break;
    end;
  if standard then exit(str);
  result := TXQueryEngineBreaker(fQueryEngine).parserEnclosedExpressionsString(str).evaluate().toString; //todo: somehow cache the parsed xquery
end;

function THtmlTemplateParser.debugMatchings(const width: integer): string;
begin
  result := debugMatchings(width, true, ['*']);
end;

function THtmlTemplateParser.debugMatchings(const width: integer; includeText: boolean; includeAttributes: array of string): string;
var res: TStringArray;
    template: TTemplateElement;
    html: TTreeNode;
    LINK, NOLINK, EMPTY: String;
    tsl, hsl: TStringArray;
    templateIndent, htmlIndent: integer;
    tempTemplateIndent, tempHTMLIndent: String;

  procedure updateIndentation(element: TTreeNode; var count: integer; var cache: string);
  begin
    if element.typ in TreeNodesWithChildren then count+=1
    else if element.typ = tetClose then count-=1;
    cache := strDup(' ', min(width div 2, count));
  end;

  function htmlToString(): string;
  begin
    if (length(includeAttributes) = 1) and (includeAttributes[0] = '*') then result := html.toString
    else result := html.toString(includeText, includeAttributes);
  end;

  procedure printHTMLUntil(endElement: TTreeNode);
  var
    i: Integer;
  begin
    while (html <> nil) and (html <> endElement) do begin
      hsl := strWrapSplit(htmlToString(), width - length(tempHTMLIndent));
      for i:=0 to high(hsl) do arrayAdd(res, tempHTMLIndent + hsl[i]);
      updateIndentation(html, htmlIndent, tempHTMLIndent);
      html := html.next;
    end;
  end;

var i: Integer;

begin
  LINK :=   ' <---- ';
  NOLINK := '       ';
  EMPTY := strDup(' ', width) + NOLINK;

  tempTemplateIndent:='';tempHTMLIndent:='';templateIndent:=0; htmlIndent:=0;

  setlength(res, 0);
  template := TTemplateElement(FTemplate.getLastTree.next);
  if template <> nil then template := template.templateNext;
  html := FHtmlTree;
  if html <> nil then html := html.next;
  while template <> nil do begin
    tsl := strWrapSplit(template.toString(), width - length(tempTemplateIndent));
    if template.match = nil then begin
      for i:=0 to high(tsl) do arrayAdd(res, EMPTY + tempTemplateIndent + tsl[i])
    end else begin
      if (html <> nil) and (template.match.offset > html.offset) then
        printHTMLUntil(template.match);
      if html = template.match then begin
        hsl := strWrapSplit(htmlToString(), width - length(tempHTMLIndent));
        for i:=0 to min(high(hsl), high(tsl)) do arrayAdd(res, tempHTMLIndent + hsl[i] + strDup(' ', width - length(hsl[i]) - length(tempHTMLIndent) ) + LINK + tempTemplateIndent + tsl[i]);
        for i:=length(hsl) to high(tsl) do arrayAdd(res, EMPTY + tempTemplateIndent + tsl[i]);
        for i:=length(tsl) to high(hsl) do arrayAdd(res, tempHTMLIndent + hsl[i]);
        updateIndentation(html, htmlIndent, tempHTMLIndent);
        html := html.next;
      end;
    end;
    updateIndentation(template, templateIndent, tempTemplateIndent);
    template := template.templateNext;
  end;
  arrayAdd(res, '<!--template end-->');
  printHTMLUntil(nil);
  result := strJoin(res, LineEnding);
end;






function guessExtractionKind(e: string): TExtractionKind;
  function checkWords(first: string; second: array of string): boolean;
  var
    temp: PChar;
    i: Integer;
  begin
    if length(e) < length(first) + 1 then exit(false);
    if not strBeginsWith(e, first) then exit(false);
    if not (e[length(first)+1] in [#1..#32]) then exit(false);
    if length(second) = 0 then exit(true);
    temp := @e[length(first)+1];
    while temp^ in [#1..#32] do inc(temp); //skip additional whitespace
    for i:= 0 to high(second) do
      if strBeginsWith(temp, second[i]) then exit(true);
    exit(false);
  end;
var
  dots: Integer;
  i: Integer;
begin
  { try to detect the type of an extract expression:
    Template:  if it is an xml file, i.e. starts with a <
    CSS:       If it contains many # or .    i.e. if there is a [#.] before any other non letter/space character
    XQuery:    If it starts with a XQuery only command (i.e. xquery version, declare function, ...)
    XPath:     otherwise
  }


  if (e = '') or (e = '.' {just context item}) then exit(ekXPath3);
  if e[1] in [#0..#32] then e := trim(e);
  if (e[1] = '<') then exit(ekPatternHTML);

  if e[1] = '#' then exit(ekCSS);

  if checkWords('xquery', ['version', 'encoding']) or checkWords('typeswitch', []) or checkWords('import', ['module', 'schema']) or
     checkWords('module', ['namespace']) or
     checkWords('declare', ['function', 'variable', 'namespace', 'default', 'boundary-space', 'base-uri', 'option', 'construction', 'copy-namespace']) or
     checkWords('for', ['sliding', 'tumbling'])
     then
    exit(ekXQuery3);

  result := ekXPath3;

  dots := 0;
  for i := 1 to length(e) do
    case e[i] of
      'a'..'z','A'..'Z',#1..#32: ;
      '#': exit(ekCSS);
      '.': if ((i = 1) or (e[i-1] in ['a'..'z','A'..'Z'])) and ((i = length(e)) or (e[i+1] in ['a'..'z','A'..'Z'])) then
         dots+=1;
      else exit(ekXPath3);
    end;
  if dots > 0 then exit(ekCSS)
  else exit(ekXPath3);
end;


function xqFunctionMatches(const context: TXQEvaluationContext; argc: sizeint; argv: PIXQValue): IXQValue;
var temp: THtmlTemplateParser;
    template, html: IXQValue;
    cols: TXQVariableChangeLog;
    tempobj: TXQValueObject;
    i: Integer;
    list: TXQVList;
begin
  requiredArgCount(argc, 2);
  list := TXQVList.create();
  try
    temp := THtmlTemplateParser.create; //TODO: optimize
    try
      temp.TemplateParser.parsingModel:=pmHTML;
      temp.TemplateParser.repairMissingStartTags:=false;

      temp.QueryEngine.StaticContext.Free;
      temp.QueryEngine.StaticContext := context.staticContext.clone();
      temp.QueryEngine.staticContext.sender := temp.QueryEngine;
      temp.FQueryContext.staticContext := temp.QueryEngine.StaticContext;
      temp.KeepPreviousVariables:=kpvForget;
      temp.OutputEncoding:=context.staticContext.stringEncoding;
      for template in argv[0] do begin
        if template is TXQValueString then temp.parseTemplate(template.toString)
        else if template is TXQValueNode then temp.parseTemplate(template.toNode.outerXML())
        else raise EXQEvaluationException.Create('pxp:PATTERN', 'Invalid type for patter. Expected node or string, but got: '+template.toXQuery());
        for html in argv[1] do begin
          if not (html is TXQValueNode) then
            raise EXQEvaluationException.Create('pxp:PATTERN', 'Invalid type for matched node. Expected node or string, but got: '+html.toXQuery());
          temp.FHtmlTree := html.toNode;
          if not temp.matchLastTrees then raise EXQEvaluationException.Create('pxp:TEMPLATE', 'Failed to match pattern to html');
          cols := temp.VariableChangeLogCondensed.collected;
          try
            if (cols.count = 1) and (cols.getName(0) = temp.UnnamedVariableName) then
              list.add(cols.get(0))
            else begin
              tempobj := TXQValueObject.create();
              for i := 0 to cols.count - 1 do
                tempobj.setMutable(cols.getName(i), cols.get(i));
              list.add(tempobj);
            end;
          finally
            cols.free;
          end;
        end;
      end;
    finally
      temp.free;
    end;
  except
    list.free;
    raise;
  end;
  xqvalueSeqSqueezed(result, list)
end;

function patternMatcherParse(const context: TXQStaticContext; data: string): TXQTermPatternMatcher;
var temp: THtmlTemplateParser;
begin
  temp := THtmlTemplateParser.create;
  if data[length(data)] in ['*','?','+','}'] then data := '<t:if>'+data+'</t:if>'; //allow count specifier at the end
  temp.parseTemplate(data);
  temp.QueryEngine.Free;
  temp.fQueryEngine := context.sender;
  temp.FQueryContext.staticContext := context;
  temp.initializeCaches;
  result := TXQTermPatternMatcher.Create;
  result.node := temp.TemplateTree;
  temp.GetTemplateRealVariableDefinitions(result.vars, result.hasDefaultVariable);
  result.contextDependancies := temp.GetTemplateContextDependencies - [xqcdFocusItem,xqcdFocusPosition,xqcdFocusLast]{<-focus is the matched document, not the outside document};
  temp.FTemplate.OwnedTrees.Clear;
  temp.FQueryEngine := nil;
  temp.free;
end;

function patternMatcherMatch(template, data: TTreeNode; var context: TXQEvaluationContext; throwExceptions: boolean = false): TXQVariableChangeLog;
var temp: THtmlTemplateParser;
  oldEngine: TXQueryEngine;
  queryVarLog: TXQVariableChangeLog;
begin
  if data = nil then exit(nil);
  temp := THtmlTemplateParser.create;
  oldEngine := temp.FQueryEngine;
  queryVarLog := context.staticContext.sender.VariableChangelog;
  context.staticContext.sender.VariableChangelog := oldEngine.VariableChangelog;
  temp.UnnamedVariableName := '$';
  temp.FQueryEngine := context.staticContext.sender;
  temp.FQueryContext := context;
  temp.ParsingExceptions := false;
  temp.FTemplate.OwnedTrees.Add(template);
  temp.FHTML.OwnedTrees.Add(data);
  temp.FHtmlTree := data; //todo: why is that not read from fhtml?
  try
    temp.resetAttributeMatching;
    if temp.matchLastTrees then begin
      result := temp.variableChangeLog;
      temp.variableChangeLog.parentLog := nil;
      temp.FVariableLog := nil;
      oldEngine.VariableChangelog := nil;
    end else begin
      if throwExceptions then raise EXQEvaluationException.Create('pxp:PATTERN', 'Failed to match pattern to data');;
      result := nil;
    end;
  finally
    context.staticContext.sender.VariableChangelog := queryVarLog;
    temp.FTemplate.OwnedTrees.Clear;
    temp.FHTML.OwnedTrees.Clear;
    temp.FQueryEngine := oldengine;
    temp.free;
  end;
end;

procedure visitActionMerge(var result: TXQTerm_VisitAction; merge: TXQTerm_VisitAction); inline;
begin
  case merge of
    xqtvaAbort: result := merge;
    xqtvaNoRecursion: if result <> xqtvaAbort then result := merge;
    xqtvaContinue: ;
  end;
end;

function patternMatcherVisit(const template: TXQTermPatternMatcher; visitor: TXQTerm_Visitor): TXQTerm_VisitAction;
var
  t: TTemplateElement;
begin
  result := xqtvaContinue;
  if template.node is TTreeDocument then t := template.node.next as TTemplateElement
  else t := template.node as TTemplateElement;
  while t <> nil do begin
    with t do begin
      if test <> nil then visitActionMerge(result, test.visit(visitor, template));
      if condition <> nil then visitActionMerge(result,condition.visit(visitor, template));
      if valuepxp <> nil then visitActionMerge(result,valuepxp.visit(visitor, template));
      if source <> nil then visitActionMerge(result,source.visit(visitor, template));
      if min <> nil then visitActionMerge(result,min.visit(visitor, template));
      if max <> nil then visitActionMerge(result,max.visit(visitor, template));
      if varname <> nil then visitActionMerge(result,varname.visit(visitor, template));
      if ignoreSelfTest <> nil then visitActionMerge(result,ignoreSelfTest.visit(visitor, template));
    end;
    t := t.templateNext;
  end;
end;


var module: TXQNativeModule;

initialization

module := TXQueryEngine.findNativeModule(XMLNamespaceURL_MyExtensionsMerged);
module.registerFunction('match', 2, 2, @xqFunctionMatches, []);
xquery.patternMatcherParse:=@patternMatcherParse;
xquery.patternMatcherMatch:=@patternMatcherMatch;
xquery.patternMatcherVisit:=@patternMatcherVisit;

end.



