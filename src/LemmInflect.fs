module LemmInflect
// Implements https://lemminflect.readthedocs.io/en/latest/inflections/

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch

//for node compatibility
importSideEffects "isomorphic-fetch"

[<StringEnum>]

type UPOS = 
    /// adjective
    | [<CompiledName("ADJ")>] ADJ  
    /// adposition
    | [<CompiledName("ADP")>] ADP 
    /// adverb 
    | [<CompiledName("ADV")>] ADV  
    /// auxiliary
    | [<CompiledName("AUX")>] AUX  
    /// coordinating conjunction
    | [<CompiledName("CCONJ")>] CCONJ 
    /// determiner
    | [<CompiledName("DET")>] DET  
    /// interjection
    | [<CompiledName("INTJ")>] INTJ 
    /// noun 
    | [<CompiledName("NOUN")>] NOUN  
    /// numeral
    | [<CompiledName("NUM")>] NUM  
    /// particle
    | [<CompiledName("PART")>] PART 
    /// pronoun 
    | [<CompiledName("PRON")>] PRON  
    /// proper noun
    | [<CompiledName("PROPN")>] PROPN  
    /// punctuation
    | [<CompiledName("PUNCT")>] PUNCT  
    /// subordinating conjunction
    | [<CompiledName("SCONJ")>] SCONJ  
    /// symbol
    | [<CompiledName("SYM")>] SYM  
    /// verb
    | [<CompiledName("VERB")>] VERB  
    /// other
    | [<CompiledName("X")>] X  

type PennPOS =
    ///Coordinating conjunction
    | [<CompiledName("CC")>] CC 
    ///Cardinal number
    | [<CompiledName("CD")>] CD 
    ///Determiner
    | [<CompiledName("DT")>] DT 
    ///Existential there
    | [<CompiledName("EX")>] EX 
    ///Foreign word
    | [<CompiledName("FW")>] FW 
    ///Preposition or subordinating conjunction
    | [<CompiledName("IN")>] IN 
    ///Adjective
    | [<CompiledName("JJ")>] JJ 
    ///Adjective, comparative
    | [<CompiledName("JJR")>] JJR 
    ///Adjective, superlative
    | [<CompiledName("JJS")>] JJS 
    ///List item marker
    | [<CompiledName("LS")>] LS 
    ///Modal
    | [<CompiledName("MD")>] MD 
    ///Noun, singular or mass
    | [<CompiledName("NN")>] NN 
    ///Noun, plural
    | [<CompiledName("NNS")>] NNS 
    ///Proper noun, singular
    | [<CompiledName("NNP")>] NNP 
    ///Proper noun, plural
    | [<CompiledName("NNPS")>] NNPS 
    ///Predeterminer
    | [<CompiledName("PDT")>] PDT 
    ///Possessive ending
    | [<CompiledName("POS")>] POS 
    ///Personal pronoun
    | [<CompiledName("PRP")>] PRP 
    ///Possessive pronoun
    | [<CompiledName("PRP$")>] PRPdollar
    ///Adverb
    | [<CompiledName("RB")>] RB 
    ///Adverb, comparative
    | [<CompiledName("RBR")>] RBR 
    ///Adverb, superlative
    | [<CompiledName("RBS")>] RBS 
    ///Particle
    | [<CompiledName("RP")>] RP 
    ///Symbol
    | [<CompiledName("SYM")>] SYM 
    ///to
    | [<CompiledName("TO")>] TO 
    ///Interjection
    | [<CompiledName("UH")>] UH 
    ///Verb, base form
    | [<CompiledName("VB")>] VB 
    ///Verb, past tense
    | [<CompiledName("VBD")>] VBD 
    ///Verb, gerund or present participle
    | [<CompiledName("VBG")>] VBG 
    ///Verb, past participle
    | [<CompiledName("VBN")>] VBN 
    ///Verb, non-3rd person singular present
    | [<CompiledName("VBP")>] VBP 
    ///Verb, 3rd person singular present
    | [<CompiledName("VBZ")>] VBZ 
    ///Wh-determiner
    | [<CompiledName("WDT")>] WDT 
    ///Wh-pronoun
    | [<CompiledName("WP")>] WP 
    ///Possessive wh-pronoun
    | [<CompiledName("WP$")>] WPdollar
    ///Wh-adverb 
    | [<CompiledName("WRB")>] WRB 

let lemmInflectEndpoint = "https://lemminflect.olney.ai/api/"

/// This methods aggregates getAllLemmas and getAllLemmasOOV. 
/// It first tries to find the lemma using the dictionary based lookup. 
/// If no forms are available, it then tries to find the lemma using the rules system. 
/// If a Penn Tag is available, it is best practice to first call isTagBaseForm (below), 
/// and only call this function if that is False. 
/// Doing this will eliminate potentials errors from lemmatizing a word already in lemma form.
/// Arguments
/// word: word to lemmatize
/// upos: Universal Dependencies part of speech the return is limited to
/// lemmatize_oov: Allow the method to use the rules based lemmatizer for words not in the dictionary
let getLemma(word: string)(upos :UPOS)(lemmatize_oov : bool) : JS.Promise<Result<string,FetchError>> =
    promise {
        return! Fetch.tryPost( lemmInflectEndpoint + "getLemma", {| word=word; upos=upos; lemmatize_oov = lemmatize_oov  |}, caseStrategy = SnakeCase)
    }
/// Returns lemmas for the given word. 
/// The format of the return is a dictionary where each key is the upos tag 
/// and the value is a tuple of possible spellings.
/// Arguments
/// word: word to lemmatize
/// upos: Universal Dependencies part of speech tag the returned values are limited to
let getAllLemmas(word: string)(upos :UPOS) : JS.Promise<Result<Map<UPOS,string[]>,FetchError>> =
    promise {
        return! Fetch.tryPost( lemmInflectEndpoint + "getAllLemmas", {| word=word; upos=upos |}, caseStrategy = SnakeCase)
    }
/// Similar to getAllLemmas except that the rules system is used for lemmatization, instead of the dictionary. The return format is the same as well.
/// Arguments
/// word: word to lemmatize
/// upos: Universal Dependencies part of speech tag the returned values are limited to
let getAllLemmasOOV(word: string)(upos :UPOS) : JS.Promise<Result<Map<UPOS,string[]>,FetchError>> =
    promise {
        return! Fetch.tryPost( lemmInflectEndpoint + "getAllLemmasOOV", {| word=word; upos=upos |}, caseStrategy = SnakeCase)
    }
/// Returns True or False if the Penn Tag is a lemma form. This is useful since lemmatizing a lemma can lead to errors. The upos tags used in the above methods don't have enough information to determine this, but the Penn tags do.
/// Arguments
/// tag: Penn Treebank tag
let isTagBaseForm(tag: PennPOS) : JS.Promise<Result<bool,FetchError>> =
    promise {
        return! Fetch.tryPost( lemmInflectEndpoint + "isTagBaseForm", {| tag=tag |}, caseStrategy = SnakeCase)
    }
/// The method returns the inflection for the given lemma based on te PennTreebank tag. 
/// It first calls getAllInflections and if none were found, calls getAllInflectionsOOV. 
/// The flag allows the user to disable the rules based inflections. 
/// The return from the method is a tuple of different spellings for the inflection.
/// Arguments
/// lemma: the word to inflect
/// tag: the Penn-Treebank tag
/// inflect_oov: if False the rules sytem will not be used.
let getInflection(lemma: string)(tag :PennPOS)(inflect_oov : bool) : JS.Promise<Result<string,FetchError>> =
    promise {
        return! Fetch.tryPost( lemmInflectEndpoint + "getInflection", {| lemma=lemma; tag=tag; inflect_oov = inflect_oov  |}, caseStrategy = SnakeCase)
    }
/// This method does a dictionary lookup of the word and returns all lemmas. 
/// Optionally, the upos tag may be used to limit the returned values to a specific part-of-speech. 
/// The return value is a dictionary where the key is the Penn Treebank tag 
/// and the value is a tuple of spellings for the inflection.
/// Arguments
/// lemma: the word to inflect
/// upos: Universal Dependencies part of speech tag the returned values are limited to
let getAllInflections(lemma: string)(upos :UPOS) : JS.Promise<Result<Map<PennPOS,string[]>,FetchError>> =
    promise {
        return! Fetch.tryPost( lemmInflectEndpoint + "getAllInflections", {| lemma=lemma; upos=upos |}, caseStrategy = SnakeCase)
    }
/// Similary to getAllInflections, but uses the rules system to inflect words.
/// Arguments
/// lemma: the word to inflect
/// upos: Universal Dependencies part of speech tag the returned values are limited to
let getAllInflectionsOOV(lemma: string)(upos :UPOS) : JS.Promise<Result<Map<PennPOS,string[]>,FetchError>> =
    promise {
        return! Fetch.tryPost( lemmInflectEndpoint + "getAllInflectionsOOV", {| lemma=lemma; upos=upos |}, caseStrategy = SnakeCase)
    }