# DSL for a Football Match  

## Domain  
The chosen domain is a **football match**.  

- Players must first **enter** the field before they can take part in actions.  
- A **play** is led by one player and may contain several actions such as passes, dribbles, fouls, steals, shots, and goals.  
- At any point, a **substitution** can be made to replace a player with another.  
- It is possible to **inspect** the state of the match:  
  - Show which players are currently active on the field.  
  - Show the statistics of an individual player (e.g., passes, shots, goals, fouls).  
- The command `dump examples` always prints a set of predefined example plays and commands.  

This DSL allows describing the sequence of events of a football match in a structured way.  

---

## Main Entities  
- **Player**: identified by their name (`String`).  
- **Actions**:  
  - `pass` – a player passes the ball to another player.  
  - `dribble` – a player attempts to dribble (success or fail).  
  - `foul` – a player commits a foul on another, optionally with a yellow or red card.  
  - `shot` – a player shoots at goal.  
  - `goal` – a player scores, assisted by another player.  
  - `steal` – a player steals the ball.  
- **Play**: a sequence of actions starting with `play <player> with ...`.  

---

## Example DSL Commands  
1. `enter "Messi"`  
2. `enter "Di María"`  
3. `play "Messi" with pass "Messi" to "Di María" and dribble "Di María" success and shot "Di María" and goal "Di María" assisted-by "Messi"`  
4. `substitution "Mbappé" for "Griezmann"`  
5. `play "Messi" with pass "Messi" to "Griezmann" and goal "Griezmann" assisted-by "Messi"`  
6. `show active players`  
7. `show stats "Messi"`  
8. `dump examples`  

---

## Grammar (BNF)  

```bnf
<program> ::= <command> | <command> <program>

<command> ::= <play>
            | <enter>
            | <substitution>
            | <showstats>
            | <showactiveplayers>
            | "dump examples"

<enter> ::= " enter " <string>

<substitution> ::= " substitution " <string> " for " <string>

<showstats> ::= " show stats " <string>

<showactiveplayers> ::= " show active players "

<play> ::= " play " <string> " with" <actions>

<actions> ::= <action>
            | <action> " and" <actions>

<action> ::= " pass " <string> " to " <string>
           | " dribble " <string> " has " <dribbleresult>
           | " hardfoul " <string> " on " <string> " " <card>
           | " foul " <string> " on " <string> 
           | " shot " <string>
           | " goal " <string> " assisted-by " <string>
           | " steal " <string> " from " <string>

<dribbleresult> ::= "succeed" | "failed"

 
<card> ::= "yellow" | "red"


<string> ::= "\"" <chars> "\""

<chars> ::= <char> | <char> <chars>

<char> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j"
         | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t"
         | "u" | "v" | "w" | "x" | "y" | "z"
         | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
         | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T"
         | "U" | "V" | "W" | "X" | "Y" | "Z"
         | " " 