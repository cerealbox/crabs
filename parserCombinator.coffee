class Parser
    constructor : (@parse) ->
    exec : (str, offset = 0) -> 
        result = @parse(str, offset)
        if result.length
           [{match}] = result
           match
        else
           result
    bind : (f) ->
        new Parser(
            (str, index) =>
                results = []
                for {match, offset} in @parse(str, index)
                    nextMatches = f(match).parse(str, offset)
                    results.push(nextMatches...)
                results)
    map : (f) ->
        new Parser(
            (str, index) =>
                results = @parse(str, index)
                results.map(({match,offset}) -> {match:f(match), str, offset}))
    filter : (f) ->
        new Parser(
            (str, index) =>
                results = @parse(str, index)
                results.filter(({match}) -> f(match)))
    many : () -> @many1().or(Parser.return([]))
    many1 : () -> @bind((match) => @many().bind((listMatch) -> Parser.return([match, listMatch...])))
    combine : ({parse}) -> new Parser((str, offset) => [@parse(str, offset)..., parse(str, offset)...])
    or : (parser) -> 
        new Parser((str, offset) => 
            list = @combine(parser).parse(str, offset)
            if list.length
                list.length = 1
            list)

Parser.return = (x) -> new Parser((str, offset) -> [{match: x, offset}])
Parser.zero = new Parser( -> [])

Parser.match = do ->
  #[[[1,2],[3]],[4]]
  unwrap = 
        ([a,b]) ->
            if a instanceof Array
                [a..., b]
            else
                [a, b]

    (args..., f) ->
        args
            .map((arg) -> 
              if typeof arg is "string"
                Parser.string(arg)
              else if arg instanceof RegExp
                Parser.regex(arg)
              else 
                arg)
            .reduce((acc, i) ->
                acc.bind((acc) ->
                    i.bind((i) ->
                        Parser.return([acc,i]))))
            .map((i) -> f(unwrap(i)...))

Parser.char = 
  new Parser((string, offset) ->
    if offset < string.length
      [{match:string.charAt(offset), offset:offset + 1}]
    else
      [])

Parser.string = (str) ->
  new Parser((string, offset) ->
    if string.lastIndexOf(str,offset) is offset
      [{match: str.substr(offset, str.length), offset : offset + str.length}]
    else
      [])

Parser.regex = (re) ->
  if (re.source and re.source.charAt(0) isnt "^")
    flags = [[re.global,"g"],[re.ignoreCase,"i"],[re.multiline, "m"]]
    flagsString = "" 
    for [flag,value] in flags
      if (flag)
        flagsString += value
          
    re = new RegExp("^" + re.source, flagsString)

  new Parser( (string, offset) ->
    re.lastIndex = offset
    results = re.exec string
    if results
      [{match: results, offset: offset + results[0].length}]
    else
      []
  )

console.log (Parser.regex /[$_a-zA-Z][a-zA-Z0-9$_]*/i).exec("9$34334")

#Parser.match(/[$_a-z][a-z0-9$_]*/i, "some string", Parser.string("something else").many()
#Parser.regex(/[$_a-zA-Z][a-zA-Z0-9$_]*/i)
#customer[22,44,77]

JSONPath = do ->

  stringLiteralChar =
    Parser.char
        .filter((c) -> c != "\\")
        .or(Parser.match("\\", Parser.char, (slash, c) -> slash + c))

  stringLiteral = 
    Parser.match(
      '"', 
      stringLiteralChar.filter((c) -> c isnt '"').many(), 
      '"', 
      (_,strChars,_) -> eval('"' + strChars.join("") + '"'))
  
  index = Parser.regex(/0-9+/).map(([match]) -> parseInt(match))
  slice = Parser.match(index, ":", index, (from,_,to) -> {from, to})
  identifier = Parser.regex(/[$_a-z][a-z0-9$_]*/i).map(([result]) -> result)

  indexerSegment = stringLiteral.or(slice).or(index)
  
  indexerSegments = 
    Parser.match(
      indexerSegment, 
      Parser.match(",", indexerSegment, (_, segment) -> segment).many(),
      (indexerSegment, rest) -> 
        rest.shift(indexerSegment)
        rest)
  
  indexerExpression = Parser.match("[", indexerSegments, "]", (_,indexerSegments,_) -> indexerSegments)
  { parser : indexerExpression.or(identifier) }

#alert (Parser.string "h").many().exec "hhhhhhhhhdddsdfsdfsdk"
#alert JSON.stringify ((Parser.string "h").parse "hhhhhdfsdf")

alert(JSON.stringify( (Parser.string "hi").bind((x) -> Parser.return "HI").exec "hiyo" ))

#alert(JSONPath.parser.exec '["customer","test"]')
