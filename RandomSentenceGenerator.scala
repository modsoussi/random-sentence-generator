/* Problem Set #8. Problem 1.
 * (c) 2016. modsousi.
 */

import java.io.FileInputStream;
import java.util.Scanner;
import scala.util.Random;

// can change to immutable._ if you wish...
import scala.collection.mutable.HashMap; 
import scala.List;

/*
 * Abstract class the all expandable parts of
 * a grammar extend (Terminal, NonTerminal, Production, Definition).
 */
abstract class GrammarElement {
  
  /**
   * Expand the grammar element as part of a random 
   * derivation.  Use grammar to look up the definitions
   * of any non-terminals encountered during expansion.
   */
  def expand(grammar : Grammar) : String;
  
  /**
   * Return a string representation of this grammar element.
   * This is useful for debugging.  (Even though we inherit a
   * default version of toString() from the Object superclass, 
   * I include it as an abstract method here to ensure that 
   * all subclasses provide their own implementation.)
   */
  def toString() : String;	
  
}


/**
 * Represents a grammar as a map from non-terminal names (Strings) to
 * Defintions.
 */
class Grammar {

  var gram: HashMap[String, Definition] = HashMap();

  // add a new non-terminal, with the given definition
  def +=(nt : String, defn : Definition) = {
    gram += (nt -> defn);
  }
  
  // look up a non-terminal, and return the definition, or null
  // if not def exists.
  def apply(nt : String) : Definition = {
    gram.get(nt) match {
      case None => RandomSentenceGenerator.fail("Non-Terminal Not Found");
      case Some(d) => d;
    }
  }
  
  // Expand the start symbol for the grammar.
  def expand() : String = {
    this("<start>").expand(this);
  }
  
  // return a String representation of this object.
  override def toString() : String = {
    gram.mkString("\n");
  }
}

/* This class stores a vector of productions. Expanded by picking
 * a random Production from its vector and expanding that production.
 */
class Definition extends GrammarElement { 

  var productions : List[Production] = List();

  override def expand(grammar : Grammar) : String = {
    val ri = Random.nextInt(productions.length);
    productions(ri).expand(grammar);
  }

  override def toString() : String = {
    productions.mkString(" || ");
  }

  def addProduction(p: Production) = {
    productions = productions:+p;
  }
}

/* This class stores a list of GrammarElements. */
class Production extends GrammarElement { 

  var elems : List[GrammarElement] = List();

  override def expand(grammar : Grammar) : String = {
    elems.foldLeft("")(_ + _.expand(grammar)); // folds are awesome
  }

  override def toString() : String = {
    elems.mkString(" | ");
  }

  def addElement(e: GrammarElement) = {
    elems = elems:+ e;
  }
}

/* This class stores a non-terminal string */
class NonTerminal(nterm: String) extends GrammarElement { 

  override def expand(grammar : Grammar) : String = {
    grammar(nterm).expand(grammar);
  }

  override def toString() : String = {
    nterm;
  }
}

/* Stores a terminal String */
class Terminal(term: String) extends GrammarElement { 

  override def expand(grammar : Grammar) : String = {
    if(Character.isLetterOrDigit(term(0))){
      " " + term;
    } else {
      term
    }
  }

  override def toString() : String = {
    term;
  }
}


object RandomSentenceGenerator {
  
    /**
     * Read tokens up to the end of a production and return 
     * them as a Production.
     *
     * Parses "Production ::= [ Word ]*"
     * where word is any terminal/non-terminal.
     */
  protected def readProduction(in : Scanner) : Production = {
    
    val p = new Production();
    var currentTerminal = "";

    while (in.hasNext() && !(in.hasNext(";") || in.hasNext("\\|"))) {
      val word = in.next();

      // word is next word in production (either a Non-Terminal or Terminal).
      if(word.startsWith("<")){
        p.addElement(new NonTerminal(word));
      } else {
        p.addElement(new Terminal(word));
      }
    }

    p;
  }
  
  /**
   * Read a group of productions and return them as a Definition.
   *
   * Parses "<Definition> ::= <Production> [ '|' <Production> ]* ';'" 
   */
  def readDefinition(in : Scanner) : Definition = {
    
    val d = new Definition();
    
    val production = readProduction(in);
    d.addProduction(production);
    // production is first production for definition
    
    while (in.hasNext("\\|")) {
      expect(in, "\\|");
      val production = readProduction(in);
      d.addProduction(production);
      // production is the next production for definition
      
    }
    expect(in, ";");

    d;  			// return the new production
  }
  
  /**
   * Repeatedly read non-terminal definitions and insert them into
   * the grammar.
   *
   * Parses "<Grammar> ::= [ <Non-Terminal> '=' <Definition> ';' ]*" 
   */
  protected def readGrammar(in : Scanner) : Grammar = {
    
    // the grammar for this generator
    val grammar = new Grammar();
    
    while (in.hasNext()) {
      val name = in.next();

      expect(in, "=");
      
      val defn = readDefinition(in);
      grammar += (name, defn);
      // defn is next definition to add to grammar
      
    }

    grammar;   // return the grammar
  }
  
  /**
   * A helper method than matches s to the next token returned from
   * the scanner.  If it matches, throw away the token and get ready
   * to read the next one.  If it doesn't match, generate an error.
   * 
   * Since s is used as a regular expression, be sure to escape any
   * special characters like |, which should become \\|
   */
  protected def expect(in : Scanner, s : String) = {
    if (!in.hasNext(s)) {
      println(in.next);
      RandomSentenceGenerator.fail("expected " + s);
    }
    in.next();  // skip s
  }
  
  
  /**
   * Helper method to abort gracefully when an error occurs.
   * <p>
   * Usage: RandomSentenceGenerator.fail("Error Message");
   */
  def fail(msg : String) = {
    throw new RuntimeException(msg);
  }
  
  /**
   * Create a random sentence generator and print out
   * three random productions.
   */
  def main(args: Array[String]) = {
    val grammar = this.readGrammar(new Scanner(scala.io.Source.stdin.mkString));

    println("Grammar is: \n" + grammar);
    println(grammar.expand()); // Expansion 1
    println(grammar.expand()); // Expansion 2
    println(grammar.expand()); // Expansion 3
  }
}

