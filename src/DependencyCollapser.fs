namespace DependencyCollapser
//https://github.com/aolney/dependency-collapser

module Rules =

    open System.Collections.Generic

    let tryGetValue (collection:IDictionary<'T,'U>) key =
        match collection.TryGetValue(key) with
        | true,v -> Some(v)
        | false,_ -> None

    let toLower (s:string) =
        s.ToLower()

    type Token =
         {
            ///0-based index
            Index : int
            Word : string
            POS : string
            ///Defaults to word if empty on creation
            Lemma : string
            DependencyType : string
            Head : int
            ///The text dominated by this token //apparently unneeded
            //Span : string
         } with 
         /// If no lemma is provided, uses word as lemma
         static member Create(index, word, pos, depType, head)= 
            { Index=index; Word=word; POS=pos; Lemma=word; DependencyType=depType;Head=head} //;Span=""}
         /// Full constructor
         static member Create(index,word, pos, lemma, depType, head) = 
            { Index=index; Word=word; POS=pos; Lemma=lemma; DependencyType=depType;Head=head} //;Span=""}

    type Dependency =
         {
            Type : string
            Governor : Token
            Dependent : Token
         }

    //TODO: simplify
    type RuleToken = 
        {
            ///From index (governor)
            Index : int
            ///Governor token (see constructor)
            Token : Token
            ///To index (dependent)
            TargetIndex : int
            ///Dependent token (see constructor)
            TargetToken : Token
            ///Current dependency type (so may not match target/dependent's dependency type)
            DependencyType : string
            Durable : bool
        } with
        member this.Signature() = 
            let typeString = if this.Durable then "d" else "r" 
            typeString + this.Index.ToString() + "_" + this.TargetIndex.ToString()

    type SpecifierType = Word | POS | Lemma 

    type RelationType =  Relation | DurableRelation

    type Property =
        {
            /// The type the pattern is applied to
            Type : SpecifierType
            /// TheOLDSpecifierTypeer used to reference a matched word
            Reference : int
            /// The regular expression pattern
            Regex : string
        }

    type Relation = 
        {
            /// The type the pattern is applied to
            Type : RelationType
            /// The reference number of the word specifier of the start word (dependent)
            From : int
            /// The reference number of the word specifier of the end word (head)
            To : int 
            /// The regular expression pattern
            Regex : string
        } with
        member this.Signature() = 
            let typeString = match this.Type with | Relation -> "r" | DurableRelation -> "d"
            typeString + this.From.ToString() + "_" + this.To.ToString()


    /// Find the spanned text for a 0-based index into tokens.
    /// Note that the dependency parse heads are 1-indexed.
    let spannedText (token : Token) (tokens: list<Token>) =
        let frontier = HashSet<int>()
        frontier.Add(token.Index + 1) |> ignore
        let mutable notDone = true
        while notDone do
            notDone <- false
            for f in frontier |> List do
                for t in tokens do
                    if frontier.Contains( t.Head ) then
                        notDone <- frontier.Add( t.Index + 1) //if not present already, notDone set to true
        //
        frontier |> Seq.sort |> Seq.map( fun f -> tokens.[f-1].Word) |> String.concat " "
            
    /// Add text of dominated tokens, i.e. a span, to each token
    //let addSpans (tokens: list<Token>) =
    //    tokens |> List.map( fun t -> {t with Span=(spannedText t tokens)})

    /// Get the governing token of a token
    let governor (tokens: list<Token>) (token : Token) =
        if token.Head > 0 then
            tokens.[ token.Head - 1]
        //ROOT token is self-governing
        else
            token

    let dependenciesFromTokens tokens =
        tokens |> List.map( fun token -> { Governor= token |> governor tokens; Dependent = token; Type=token.DependencyType } )

    /// Do a regex match and return an option of the retval
    let doMatch retVal pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some(retVal) else None

    /// Match on the appropriate property
    let propertyMatch (property : Property) token =
        match property.Type with
        //| Word -> token.Span |> doMatch token.Span property.Regex //assumed span was needed b/c "getCoveredText" is called in the original, but apparently that call is scoped to the token.
        | Word -> token.Word |> doMatch token.Word property.Regex
        //| POS -> token.POS |> doMatch token.POS property.Regex 
        | POS -> token.POS |> doMatch token.Word property.Regex //This is somewhat undocumented, but often added relations have a name that is the word associated with a POS match. Currently no rules require POS for a POS match in this way.
        | Lemma -> token.Lemma |> doMatch token.Lemma property.Regex


    type Model =
        {
            From : int
            To : int
            Tokens : Dictionary<int,Token>
            Relations : ResizeArray<RuleToken>
            AddedTokens : HashSet<string>
        } with
        static member Create(from, To)= 
            { From=from; To=To; Tokens=Dictionary<int,Token>(); Relations=ResizeArray<RuleToken>();AddedTokens=HashSet<string>()}
        member this.AddRuleToken( ruleToken : RuleToken ) =
            match this.AddedTokens.Add( sprintf "%A" ruleToken ) with
            | false -> None
            | true ->
                //clone the current model with new reference types and fill them
                let newModel = { 
                    this with 
                        Tokens = Dictionary<int,Token>(this.Tokens)
                        Relations = ResizeArray<RuleToken>(this.Relations)
                        AddedTokens = HashSet<string>(this.AddedTokens) //NOTE added tokens will be identical bw original and copy
                    } 
                match this.Tokens.TryGetValue(ruleToken.Index ),  this.Tokens.TryGetValue(ruleToken.TargetIndex )  with
                | (false, token), (false, targetToken) -> 
                    newModel.Tokens.Add( ruleToken.Index, ruleToken.Token )
                    newModel.Tokens.Add( ruleToken.TargetIndex, ruleToken.TargetToken )
                    newModel.Relations.Add( ruleToken )
                    Some(newModel)
                | (true, token), (false, targetToken) when token = ruleToken.Token ->
                    newModel.Tokens.Add( ruleToken.TargetIndex, ruleToken.TargetToken )
                    newModel.Relations.Add( ruleToken )
                    Some(newModel)
                | (false, token), (true, targetToken) when targetToken = ruleToken.TargetToken ->
                    newModel.Tokens.Add( ruleToken.Index, ruleToken.Token )
                    newModel.Relations.Add( ruleToken )
                    Some(newModel)
                | _ -> 
                    for relation in this.Relations do if relation = ruleToken then newModel.Relations.Add( ruleToken )
                    //only emit a new model if it is different from the currenct model
                    let newModelString = sprintf "%A" newModel //hack: we are using types that don't have structural equality, so we fake it
                    let thisString = sprintf "%A" this
                    if newModelString = thisString then None else Some(newModel)

    type Rule =
        {
            Properties : Map<int,Property>
            Relations : Relation list
            /// The reference number of the word the new dependency orginates from (dependent)
            From : int
            /// The reference number of the word the new dependency goes to (head)
            To : int
            /// TODO: rename RelationNameTemplate. Arbitrary label of the relation name; it can contain the word,pos,or lemma matched, e.g. "relationName:the_Pos_tag_is_{p1}".
            RelationName : string
        } with
        member this.ResolvedRelationName( model : Model ) =
            //get all property/relation word matches for this model
            let matchDictionary = Dictionary<string,string>()
            this.Properties 
            |> Map.toList 
            |> List.iter( fun (i,property) -> 
                match model.Tokens.[i] |> propertyMatch property with
                | Some(matchedWords) -> matchDictionary.Add( property |> sprintf "%A", matchedWords )
                | None -> ()
            )
            this.Relations
            |> List.iter( fun relation ->
                model.Relations
                |> Seq.iter( fun ruleToken -> 
                    if relation.Signature() = ruleToken.Signature() then
                        let dependencyType = ruleToken.DependencyType
                        match dependencyType |> doMatch dependencyType relation.Regex with
                        | Some(matchedWords) ->  matchDictionary.Add( relation |> sprintf "%A", matchedWords )
                        | None -> ()
                    ()
                )
            )

            let mutable retVal = this.RelationName
            let regex = System.Text.RegularExpressions.Regex(@"\{(\w+)\}")
            
            regex.Matches(this.RelationName)
            |> Seq.cast
            |> Seq.choose( fun (m : System.Text.RegularExpressions.Match) -> 
                let id = m.Groups.[1].Value         
                if id.StartsWith("r") || id.StartsWith("d") then //only relations start with r or d
                    match this.Relations |> List.tryFind( fun relation -> id = relation.Signature() ) with
                    | Some(r) -> r |> sprintf "%A" |> tryGetValue matchDictionary
                    | None -> None
                else
                    this.Properties.[ id.Substring(1) |> System.Int32.Parse ] |> sprintf "%A" |> tryGetValue matchDictionary 

            )
            |> Seq.iter( fun s -> 
                retVal <- regex.Replace(retVal, s, 1) //replace only the first found
                )
            //
            retVal |> toLower

    type Stage =
        {
            Id : int
            Rules : Rule list
        }


    ///// Automatically created from OLDstageRuleList to better match jobimtext API
    let stageRuleList =
        [{Id = 1;
          Rules =
           [{Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "prep";}];
             From = 1;
             To = 3;
             RelationName = "prep";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = "well";}); (2, {Type = Word;
                                                            Reference = 2;
                                                            Regex = "as";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = ".*";}];
             From = 1;
             To = 2;
             RelationName = "void";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "xcomp";}; {Type = DurableRelation;
                                               From = 1;
                                               To = 2;
                                               Regex = "ccomp";}];
             From = 1;
             To = 2;
             RelationName = "void";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "conj";}];
             From = 1;
             To = 3;
             RelationName = "conj";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = DurableRelation;
                                              From = 2;
                                              To = 3;
                                              Regex = "conj";}];
             From = 1;
             To = 3;
             RelationName = "prep";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "cc";}; {Type = Relation;
                                            From = 2;
                                            To = 3;
                                            Regex = "mwe";}];
             From = 1;
             To = 2;
             RelationName = "void";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = POS;
                                            Reference = 2;
                                            Regex = "IN|TO|VBG";}); (3, {Type = Word;
                                                                         Reference = 3;
                                                                         Regex = ".*";});
                 (4, {Type = Word;
                      Reference = 4;
                      Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = DurableRelation;
                                              From = 2;
                                              To = 3;
                                              Regex = "pobj";}; {Type = DurableRelation;
                                                                 From = 2;
                                                                 To = 4;
                                                                 Regex = "mwe";}];
             From = 1;
             To = 4;
             RelationName = "mwe_helper";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "parataxis";}; {Type = DurableRelation;
                                                   From = 1;
                                                   To = 2;
                                                   Regex = "^(?!parataxis|tmod$).*";}];
             From = 1;
             To = 2;
             RelationName = "void";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "tmod";}; {Type = DurableRelation;
                                              From = 1;
                                              To = 2;
                                              Regex = "parataxis";}];
             From = 1;
             To = 2;
             RelationName = "void";}];};
         {Id = 2;
          Rules =
           [{Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "next";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = "to";});
                 (4, {Type = Word;
                      Reference = 4;
                      Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = ".*";}; {Type = Relation;
                                            From = 2;
                                            To = 3;
                                            Regex = ".*";}; {Type = Relation;
                                                             From = 3;
                                                             To = 4;
                                                             Regex = "pobj";}];
             From = 1;
             To = 4;
             RelationName = "prep_{w2}_to";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = POS;
                                            Reference = 2;
                                            Regex = "IN|TO|VBG";}); (3, {Type = Word;
                                                                         Reference = 3;
                                                                         Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = DurableRelation;
                                              From = 2;
                                              To = 3;
                                              Regex = "pobj";}];
             From = 1;
             To = 3;
             RelationName = "prep_{w2}";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "as";}); (3, {Type = Word;
                                                                  Reference = 3;
                                                                  Regex = ".*";});
                 (4, {Type = Word;
                      Reference = 4;
                      Regex = "such";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "pobj";}; {Type = Relation;
                                                                 From = 2;
                                                                 To = 4;
                                                                 Regex = "mwe";}];
             From = 1;
             To = 3;
             RelationName = "prep_such_as";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = POS;
                                            Reference = 2;
                                            Regex = "IN|TO";}); (3, {Type = Word;
                                                                     Reference = 3;
                                                                     Regex = ".*";});
                 (4, {Type = Word;
                      Reference = 4;
                      Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "pobj";}; {Type = Relation;
                                                                 From = 2;
                                                                 To = 4;
                                                                 Regex = "punct";}];
             From = 1;
             To = 4;
             RelationName = "punct";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = POS;
                                              Reference = 2;
                                              Regex = "IN|TO";}); (3, {Type = Word;
                                                                       Reference = 3;
                                                                       Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "pcomp";}];
             From = 1;
             To = 3;
             RelationName = "prepc_{w2}";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";});
                 (2, {Type = Word;
                      Reference = 2;
                      Regex = "and|or|but|nor|in|only|as|at|vs\.|&|versus|and/or";});
                 (3, {Type = Word;
                      Reference = 3;
                      Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "cc";}; {Type = Relation;
                                            From = 1;
                                            To = 3;
                                            Regex = "conj";}];
             From = 1;
             To = 3;
             RelationName = "conj_{w2}";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "not|instead|rather";});
                 (3, {Type = Word;
                      Reference = 3;
                      Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "cc";}; {Type = Relation;
                                            From = 1;
                                            To = 3;
                                            Regex = "conj";}];
             From = 1;
             To = 3;
             RelationName = "conj_negcc";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "xsubj|ref|possessive";}];
             From = 1;
             To = 2;
             RelationName = "void";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "advmod";}];
             From = 1;
             To = 3;
             RelationName = "advmod";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "dep";}];
             From = 1;
             To = 3;
             RelationName = "dep";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = "to";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "pcomp";}];
             From = 1;
             To = 3;
             RelationName = "prepc_{w2}_{w3}";}];};
         {Id = 3;
          Rules =
           [{Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = "such";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep_as";}];
             From = 1;
             To = 2;
             RelationName = "prep_such_as";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep_[^_]+";}; {Type = DurableRelation;
                                                    From = 1;
                                                    To = 2;
                                                    Regex = "prep_[^_]+_.+";}];
             From = 1;
             To = 2;
             RelationName = "void";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "conj_&";}];
             From = 1;
             To = 2;
             RelationName = "conj_and";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = ".*";}); (3, {Type = Word;
                                                                  Reference = 3;
                                                                  Regex = ".*";});
                 (4, {Type = Word;
                      Reference = 4;
                      Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 3;
                           Regex = "conj_and";}; {Type = Relation;
                                                  From = 1;
                                                  To = 2;
                                                  Regex = "pobj";}; {Type = Relation;
                                                                     From = 3;
                                                                     To = 4;
                                                                     Regex = "pobj";}];
             From = 2;
             To = 4;
             RelationName = "conj_and";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = POS;
                                            Reference = 2;
                                            Regex = "IN|TO|VBG";}); (3, {Type = Word;
                                                                         Reference = 3;
                                                                         Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "pobj";}];
             From = 1;
             To = 3;
             RelationName = "void";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "advmod";}];
             From = 1;
             To = 3;
             RelationName = "void";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "to|of|with";});
                 (3, {Type = Word;
                      Reference = 3;
                      Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep[c]?_.+";}; {Type = Relation;
                                                     From = 2;
                                                     To = 3;
                                                     Regex = "pobj";}];
             From = 1;
             To = 3;
             RelationName = "pobj";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "cc";}; {Type = DurableRelation;
                                            From = 1;
                                            To = 3;
                                            Regex = "conj_and";}];
             From = 1;
             To = 2;
             RelationName = "void";};
            {Properties =
              Map.ofList <| [(1, {Type = POS;
                        Reference = 1;
                        Regex = "CD";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "advmod";}; {Type = Relation;
                                                From = 1;
                                                To = 3;
                                                Regex = "prep_of";}];
             From = 1;
             To = 3;
             RelationName = "prep_{w2}_of";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "advmod";}; {Type = Relation;
                                                From = 2;
                                                To = 3;
                                                Regex = "prepc_of";}];
             From = 1;
             To = 3;
             RelationName = "prepc_{w2}_of";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "advmod";}; {Type = Relation;
                                                From = 1;
                                                To = 3;
                                                Regex = "prep_to";}];
             From = 1;
             To = 3;
             RelationName = "prep_{w2}_to";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "advmod";}; {Type = Relation;
                                                From = 2;
                                                To = 3;
                                                Regex = "prep_to";}];
             From = 1;
             To = 3;
             RelationName = "prep_{w2}_to";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "[aA]ccording|[dD]ue";});
                 (3, {Type = Word;
                      Reference = 3;
                      Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = ".*";}; {Type = Relation;
                                            From = 2;
                                            To = 3;
                                            Regex = "prep_to";}];
             From = 1;
             To = 3;
             RelationName = "prep_{w2}_to";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "addition";}); (3, {Type = Word;
                                                                        Reference = 3;
                                                                        Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep_in";}; {Type = Relation;
                                                 From = 2;
                                                 To = 3;
                                                 Regex = "prep_to";}];
             From = 1;
             To = 3;
             RelationName = "prep_in_addition_to";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "addition";}); (3, {Type = Word;
                                                                        Reference = 3;
                                                                        Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep_in";}; {Type = Relation;
                                                 From = 2;
                                                 To = 3;
                                                 Regex = "prep_of";}];
             From = 1;
             To = 3;
             RelationName = "prep_in_front_of";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "[aA]long|[tT]ogether";});
                 (3, {Type = Word;
                      Reference = 3;
                      Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "advmod";}; {Type = Relation;
                                                From = 2;
                                                To = 3;
                                                Regex = "prep_with";}];
             From = 1;
             To = 3;
             RelationName = "prep_{w2}_with";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = "with";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prepc_along|prepc_together";}];
             From = 1;
             To = 2;
             RelationName = "{r1_2}_with";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = "out|off";}); (3, {Type = Word;
                                                                       Reference = 3;
                                                                       Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prt";}; {Type = Relation;
                                             From = 1;
                                             To = 3;
                                             Regex = "prep_of";}];
             From = 1;
             To = 3;
             RelationName = "prep_{w2}_of";};
            {Properties = Map.ofList <| [(1, {Type = Word;
                                    Reference = 1;
                                    Regex = ".*";}); (2, {Type = Word;
                                                          Reference = 2;
                                                          Regex = "of";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prepc_as|prepc_out";}];
             From = 1;
             To = 2;
             RelationName = "{r1_2}_of";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = ".*";}); (2, {Type = Word;
                                            Reference = 2;
                                            Regex = ".*";}); (3, {Type = Word;
                                                                  Reference = 3;
                                                                  Regex = "of|to";});
                 (4, {Type = Word;
                      Reference = 4;
                      Regex = ".*";})];
             Relations =
              [{Type = Relation;
                From = 1;
                To = 2;
                Regex = "prep_of|prep_to";}; {Type = Relation;
                                              From = 1;
                                              To = 4;
                                              Regex = "mwe_helper";}; {Type = Relation;
                                                                       From = 3;
                                                                       To = 4;
                                                                       Regex = "mwe";}];
             From = 1;
             To = 2;
             RelationName = "prep_{w4}_{w3}";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prepc_.*";}; {Type = Relation;
                                                  From = 2;
                                                  To = 3;
                                                  Regex = "pcomp";}];
             From = 1;
             To = 3;
             RelationName = "pcomp";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep[c]?_by";}; {Type = DurableRelation;
                                                     From = 1;
                                                     To = 3;
                                                     Regex = "auxpass";}];
             From = 1;
             To = 2;
             RelationName = "agent";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "vmod";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "prep_by";}];
             From = 2;
             To = 3;
             RelationName = "agent";};
            {Properties =
              Map.ofList <|
                [(1, {Type = Word;
                      Reference = 1;
                      Regex = "[fF]rom";}); (2, {Type = Word;
                                                 Reference = 2;
                                                 Regex = ".*";}); (3, {Type = Word;
                                                                       Reference = 3;
                                                                       Regex = ".*";});
                 (4, {Type = Word;
                      Reference = 4;
                      Regex = ".*";})];
             Relations = [{Type = Relation;
                           From = 1;
                           To = 2;
                           Regex = "prep_to";}; {Type = DurableRelation;
                                                 From = 3;
                                                 To = 4;
                                                 Regex = "prep_from";}];
             From = 3;
             To = 2;
             RelationName = "prep_to";}];};
         {Id = 4;
          Rules =
           [{Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "dobj|nsubj";}; {Type = DurableRelation;
                                                    From = 2;
                                                    To = 3;
                                                    Regex = "conj.*";}];
             From = 1;
             To = 3;
             RelationName = "{d1_2}";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "prep.*";}; {Type = DurableRelation;
                                                From = 2;
                                                To = 3;
                                                Regex = "conj.*";}];
             From = 1;
             To = 3;
             RelationName = "{d1_2}";};
            {Properties =
              Map.ofList <| [(1, {Type = Word;
                        Reference = 1;
                        Regex = ".*";}); (2, {Type = Word;
                                              Reference = 2;
                                              Regex = ".*";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
             Relations = [{Type = DurableRelation;
                           From = 1;
                           To = 2;
                           Regex = "nsubj";}; {Type = DurableRelation;
                                               From = 2;
                                               To = 3;
                                               Regex = "conj.*";}];
             From = 3;
             To = 2;
             RelationName = "{d1_2}";}];}]

module Collapser =

    open Rules
    open System.Collections.Generic

    ///Get candidates for replacement (dependencies) and indices for replacing them
    let getCandidatesAndIndices (rule : Rule) (dependencies : Dependency seq) =

        //for each rule relation test each dependency and return matching dependencies
        let candidates = 
            rule.Relations 
            |> List.map( fun relation -> 
                dependencies 
                |> Seq.choose( fun dep -> 
                    dep.Type |> doMatch dep relation.Regex 
                ) 
                |> Seq.toList
            )

        //confirm candidate dependencies and construct indices for matching rules
        let confirmedCandidates = ResizeArray<ResizeArray<Dependency>>()
        let ruleTokens = Dictionary<int,ResizeArray<RuleToken>>()
        let targetRuleTokens =  Dictionary<int,ResizeArray<RuleToken>>()

        for i = 0 to candidates.Length - 1 do
            confirmedCandidates.Add( new ResizeArray<Dependency>() )
            for dep in candidates.[i] do
                let from,To = rule.Relations.[i].From,rule.Relations.[i].To//Indexing with 'i' works because candidates can be duplicates; we implicitly loop over relations for non duplicates
                let headMatch = dep.Governor |> propertyMatch rule.Properties.[from]
                let dependentMatch = dep.Dependent |> propertyMatch rule.Properties.[To]
                let relationMatch = dep.Type |> doMatch null rule.Relations.[i].Regex
                match headMatch,dependentMatch,relationMatch with
                | Some(_),Some(_),Some(_) -> 
                    confirmedCandidates.[i].Add(dep)
                    //initialize empty containers
                    if not <| ruleTokens.ContainsKey(from) then ruleTokens.Add( from, ResizeArray<RuleToken>() )
                    if not <| targetRuleTokens.ContainsKey(To) then targetRuleTokens.Add( To, ResizeArray<RuleToken>() )
                    //add ruleToken to respective containers
                    let ruleToken = { Index=from; TargetIndex=To; Token= dep.Governor; TargetToken= dep.Dependent; DependencyType=dep.Type; Durable=rule.Relations.[i].Type = RelationType.DurableRelation }
                    ruleTokens.[from].Add(ruleToken)
                    targetRuleTokens.[To].Add(ruleToken)
                | _ -> ()
            ()
        ()
        //
        confirmedCandidates, ruleTokens, targetRuleTokens

    ///Build models ?representing incremental rule applications?
    let createModels (rule:Rule) (ruleTokens:Dictionary<int,ResizeArray<RuleToken>>) (targetRuleTokens:Dictionary<int,ResizeArray<RuleToken>>) =

        let models = new ResizeArray<Model>()
        models.Add( Model.Create( rule.From, rule.To ) )

        for x = 0 to rule.Properties.Count - 1 do
            for relation in rule.Relations do
                if ruleTokens.[relation.From].Count > 0 && targetRuleTokens.[relation.To].Count > 0 then
                    for fromRuleToken in ruleTokens.[relation.From] do
                        let modelCount = models.Count - 1
                        for i = 0 to modelCount do
                            match models.[i].AddRuleToken( fromRuleToken ) with
                            | Some(model) -> models.Add(model)
                            | None -> ()
            ()
        ()
        //
        models

    ///Using models, select dependencies to remove or add, given this rule
    let selectDependenciesWithModels (rule : Rule) models (dependenciesToRemove:HashSet<Token*Token>) (dependenciesToAdd : HashSet<Dependency>) =
        
        let keys = HashSet<string>() //TODO: no longer needed?

        //If models are valid, adjust the dependencies accordingly
        for model in models do
            match model.Tokens.TryGetValue(rule.From),model.Tokens.TryGetValue(rule.To) with
            | (true,governor),(true,dependent) ->
                //Original logic pretty ugly; attempting simplification
                let modelIsValid = 
                    rule.Relations 
                    |> List.forall( fun relation -> 
                        model.Tokens.ContainsKey( relation.From) && 
                        model.Tokens.ContainsKey( relation.To ) &&
                        model.Relations
                        |> Seq.exists( fun ruleToken -> 
                            ruleToken.Index = relation.From && 
                            ruleToken.TargetIndex = relation.To && 
                            (ruleToken.DependencyType |> doMatch null relation.Regex).IsSome ) )
                if modelIsValid then
                    let dependencyType = rule.ResolvedRelationName( model )
                    let key = governor.Index.ToString() + "|" + dependent.Index.ToString() + "|" + dependencyType

                    //only apply an operation once; NOTE: is this necessary if we are hashing?
                    if keys.Add(key) then
                        //collect removal operations
                        model.Relations 
                        |> Seq.filter( fun rt -> not <| rt.Durable )
                        |> Seq.iter( fun rt -> dependenciesToRemove.Add( rt.Token,rt.TargetToken) |> ignore )
                    
                        //add new dependency if type not void
                        if dependencyType <> "void" then
                            dependenciesToAdd.Add( {Type=dependencyType; Governor=governor; Dependent=dependent } ) |> ignore

            | _ -> ()

    let StanfordFormat (dependency:Dependency) =
        let governorString = 
            if dependency.Type = "root" then
                "ROOT-0"
            else 
                dependency.Governor.Word + "-" + (dependency.Governor.Index + 1).ToString()
        let dependentString = dependency.Dependent.Word + "-" + (dependency.Dependent.Index + 1).ToString()
        dependency.Type + "(" + governorString + ", " + dependentString + ")"

    ///This attempts to follow jobimtext while also cleaning it up
    let CollapseTokens( tokens : Token list) =

        //dependencies are initialized from tokens but then mutated after each stage
        let dependencies =
            tokens
            |> dependenciesFromTokens
            |> ResizeArray
            
        //Apply rules in stages
        for stage in stageRuleList do

            //Collect operations; perform *after* all rules are applied
            let dependenciesToRemove = HashSet<Token*Token>()
            let dependenciesToAdd = HashSet<Dependency>()

            //Apply rules for current stage
            for rule in stage.Rules do

                //Given this rule, for each of its relations, get candidates for replacement (dependencies) and indices for replacing them
                let confirmedCandidates, ruleTokens, targetRuleTokens = getCandidatesAndIndices rule dependencies 

                //All rule relations must have a candidate dependency or the rule will never succeed
                if confirmedCandidates |> Seq.forall( fun depList -> depList.Count > 0 ) then

                    //Build models ?representing incremental application of this rule?
                    let models = createModels rule ruleTokens targetRuleTokens

                    //NOTE: dead code with conflicting indices omitted

                    //Using models, select dependencies to remove or add, given this rule
                    selectDependenciesWithModels rule models dependenciesToRemove dependenciesToAdd

            // remove dependencies using collected information
            dependencies
            |> List.ofSeq //clone to avoid modifying the iterating collection
            |> List.iter( fun dep -> if dependenciesToRemove.Contains(dep.Governor,dep.Dependent) then dependencies.Remove(dep) |> ignore )
                                
            //add dependencies using collected information
            dependencies.AddRange( dependenciesToAdd )

        //
        tokens |> dependenciesFromTokens,
        dependencies |> Seq.sortBy( fun dep -> dep.Dependent.Index * 10 + dep.Governor.Index ) //hackish way to keep token order but further order by head when equal
         