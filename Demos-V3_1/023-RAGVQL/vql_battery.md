# VQL Question Battery — "The Mystery of the Forgotten Library"

Dataset: 12 nodes (NODE #0 to #11)
Metadata fields: `path`, `Posicion`, `fecha`, `FechaDoc`, `tipo`, `autor`
Author: Gustavo | Type: cuento

---

## Characters reference

| Character | Role | Node(s) |
|-----------|------|---------|
| Elena Morales | Librarian, granddaughter of Don Ricardo | #0, #3, #8 |
| Don Ricardo Morales | Library founder, grandfather of Elena | #0, #6, #7 |
| Tomás Vega | Historian from the capital | #1, #3 |
| Carmen Vega | Tomás's mother, Don Ricardo's best friend | #1, #2 |
| Dr. Fernando Ruiz | Archaeology professor, National University | #3, #4, #10 |
| Lucía Mendoza | Fernando's wife, Elena's cousin | #4 |
| Martín López | Library caretaker (30 years) | #5, #6 |
| Fray Antonio Domínguez | Spanish missionary, Don Ricardo's mentor | #7 |
| Pablo Morales | Elena's brother, systems engineer | #8, #9 |
| Ana Torres | Pablo's fiancée, graphic designer | #9 |
| Roberto Jiménez | Mayor, Dr. Ruiz's brother-in-law | #10 |

---

## TYPE 1 — FACTUAL
> Strategy: HYBRID with high lexical weight · low top-k · high threshold · RERANK

### Q1. Who founded the Municipal Library?
```vql
SEARCH "who founded the Municipal Library"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
RERANK "founder of the library name"
THRESHOLD GLOBAL 0.80
RETURN TEXT, SCORE
LIMIT 2
```
**Expected:** Don Ricardo Morales (Node #0)

---

### Q2. What is Dr. Fernando Ruiz's profession?
```vql
SEARCH "Dr. Fernando Ruiz profession job"
USING HYBRID WITH WEIGHTS (0.3, 0.7)
RERANK "Fernando Ruiz occupation"
THRESHOLD LEXICAL 0.75
RETURN TEXT, SCORE
LIMIT 1
```
**Expected:** Professor of archaeology at the National University (Node #3)

---

### Q3. Who is Elena Morales's cousin?
```vql
SEARCH "Elena cousin family relation"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
RERANK "Elena Morales cousin name"
THRESHOLD GLOBAL 0.78
RETURN TEXT, SCORE
LIMIT 2
```
**Expected:** Lucía Mendoza (Node #4)

---

### Q4. Who was Pablo Morales engaged to?
```vql
SEARCH "Pablo Morales engaged fiancée partner"
USING HYBRID WITH WEIGHTS (0.3, 0.7)
RERANK "Pablo Morales fiancée name"
THRESHOLD LEXICAL 0.72
RETURN TEXT, SCORE
LIMIT 1
```
**Expected:** Ana Torres (Node #9)

---

### Q5. What was the hidden manuscript that was found?
```vql
SEARCH "what was the hidden manuscript document"
USING HYBRID WITH WEIGHTS (0.5, 0.5)
RERANK "manuscript identity description"
THRESHOLD GLOBAL 0.75
RETURN TEXT, SCORE
LIMIT 2
```
**Expected:** Diary written by Fray Antonio Domínguez (Node #7)

---

### Q6. Who was Don Ricardo's mentor?
```vql
SEARCH "Don Ricardo mentor teacher guide"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
RERANK "mentor of Don Ricardo name"
THRESHOLD GLOBAL 0.78
RETURN TEXT, SCORE
LIMIT 2
```
**Expected:** Fray Antonio Domínguez (Node #7)

---

### Q7. How long had Martín López worked at the library?
```vql
SEARCH "Martín López years working library caretaker"
USING HYBRID WITH WEIGHTS (0.3, 0.7)
RERANK "how long Martin Lopez worked"
THRESHOLD LEXICAL 0.70
RETURN TEXT, SCORE
LIMIT 1
```
**Expected:** Thirty years (Node #5)

---

## TYPE 2 — COMPARATIVE
> Strategy: Two separate sub-queries · MMR diversity · medium top-k · RERANK

### Q8. Compare the roles of Elena and Tomás in finding the manuscript
```vql
SEARCH "Elena role in the investigation search"
USING HYBRID WITH WEIGHTS (0.6, 0.4)
RERANK "Elena Morales role contribution"
THRESHOLD GLOBAL 0.65
RETURN TEXT, SCORE
LIMIT 4

-- then:
SEARCH "Tomás Vega role in the investigation research"
USING HYBRID WITH WEIGHTS (0.6, 0.4)
RERANK "Tomás Vega role contribution"
THRESHOLD GLOBAL 0.65
RETURN TEXT, SCORE
LIMIT 4
```
**Expected:** Elena → curator/family member (Nodes #0, #3, #8) | Tomás → external researcher (Nodes #1, #3)

---

### Q9. What are the differences between Tomás Vega's and Dr. Ruiz's contributions?
```vql
SEARCH "Tomás Vega contribution to the manuscript search"
USING EMBEDDINGS
THRESHOLD GLOBAL 0.65
RETURN TEXT, SCORE
LIMIT 3

-- then:
SEARCH "Dr. Fernando Ruiz contribution help offered"
USING HYBRID WITH WEIGHTS (0.5, 0.5)
RERANK "Ruiz help translation"
THRESHOLD GLOBAL 0.65
RETURN TEXT, SCORE
LIMIT 3
```
**Expected:** Tomás → historian/researcher | Ruiz → translation/archaeology expert (Nodes #3, #4)

---

### Q10. Who contributed more to the discovery: Martín López or Pablo Morales?
```vql
SEARCH "Martín López secret location manuscript revelation"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
RETURN TEXT, SCORE
LIMIT 3

-- then:
SEARCH "Pablo Morales digitize help technology contribution"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
RETURN TEXT, SCORE
LIMIT 3
```
**Expected:** Martín → key (Nodes #5, #6) | Pablo → preservation (Nodes #8, #9)

---

## TYPE 3 — ANALYTICAL / EXPLANATORY
> Strategy: EMBEDDINGS · moderate top-k · RERANK with causal context · HIGHLIGHTS

### Q11. Why did Martín López reveal the secret of the manuscript?
```vql
SEARCH "why Martín López revealed the secret location manuscript"
USING EMBEDDINGS
RERANK "reason Martin Lopez revealed secret condition"
THRESHOLD GLOBAL 0.68
RETURN TEXT, HIGHLIGHTS
LIMIT 4
```
**Expected:** Don Ricardo asked him to reveal it only to a family member (Nodes #5, #6)

---

### Q12. How did the library become a historical research center?
```vql
SEARCH "how did the library become research center transformation"
USING EMBEDDINGS
RERANK "library transformation into research center causes"
THRESHOLD GLOBAL 0.65
RETURN TEXT, HIGHLIGHTS
LIMIT 5
```
**Expected:** Causal chain → manuscript → digitization → exhibition → funding → restoration (Nodes #8, #9, #10)

---

### Q13. What role did family connections play in solving the mystery?
```vql
SEARCH "family relationships connections importance mystery solving"
USING EMBEDDINGS
RERANK "family ties that enabled the discovery"
THRESHOLD GLOBAL 0.62
RETURN TEXT, HIGHLIGHTS
LIMIT 6
```
**Expected:** Multiple family ties unlocked each step (Nodes #0, #1, #4, #6, #10)

---

## TYPE 4 — EXPLORATORY / BIG PICTURE
> Strategy: EMBEDDINGS · high top-k · DEDUPLICATE SEMANTIC · no threshold floor

### Q14. Give me an overview of all the characters in the story
```vql
SEARCH "characters people persons involved in the story"
USING EMBEDDINGS
THRESHOLD GLOBAL 0.55
OPTIMIZE DEDUPLICATE SEMANTIC
RETURN TEXT, METADATA
LIMIT 12
```
**Expected:** All nodes — every chunk mentions at least one character

---

### Q15. What are the main themes of this story?
```vql
SEARCH "themes topics subjects of the story mystery library manuscript"
USING EMBEDDINGS
THRESHOLD GLOBAL 0.55
OPTIMIZE DEDUPLICATE SEMANTIC
RETURN TEXT, HIGHLIGHTS
LIMIT 12
```
**Expected:** Family heritage · colonial history · teamwork · cultural preservation

---

### Q16. What do we know about the history of the Municipal Library?
```vql
SEARCH "Municipal Library history origin background"
USING EMBEDDINGS
THRESHOLD GLOBAL 0.58
OPTIMIZE DEDUPLICATE SEMANTIC
RETURN TEXT, METADATA
LIMIT 10
```
**Expected:** Nodes #0, #5, #6, #7, #10

---

## TYPE 5 — AGGREGATION / SYNTHESIS
> Strategy: HYBRID · very high top-k · no THRESHOLD · DEDUPLICATE SEMANTIC

### Q17. List all the characters mentioned in the story
```vql
SEARCH "person name character mentioned in the story"
USING HYBRID WITH WEIGHTS (0.5, 0.5)
OPTIMIZE DEDUPLICATE SEMANTIC
RETURN TEXT, METADATA
LIMIT 50
```
**Expected:** All 12 nodes — must not miss any character

---

### Q18. What professions are represented among the characters?
```vql
SEARCH "profession occupation job work career character"
USING EMBEDDINGS
OPTIMIZE DEDUPLICATE SEMANTIC
RETURN TEXT
LIMIT 50
```
**Expected:** Librarian · historian · archaeologist · caretaker · engineer · graphic designer · mayor · missionary

---

### Q19. List all family relationships described in the story
```vql
SEARCH "family relation relative cousin brother wife married grandfather friend"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
OPTIMIZE DEDUPLICATE SEMANTIC
RETURN TEXT, HIGHLIGHTS
LIMIT 50
```
**Expected:** Elena↔Don Ricardo · Fernando↔Lucía · Lucía↔Elena · Pablo↔Elena · Jiménez↔Ruiz (Nodes #0, #1, #4, #8, #10)

---

## TYPE 6 — TEMPORAL / EVOLUTIONARY
> Strategy: EMBEDDINGS or HYBRID · WHERE FechaDoc · REORDER CHRONOLOGICAL

### Q20. How did the story events unfold over time? (documents before 2015)
```vql
SEARCH "events sequence what happened in the story"
USING EMBEDDINGS
WHERE FechaDoc < "2015-01-01"
OPTIMIZE REORDER CHRONOLOGICAL
RETURN TEXT, METADATA
LIMIT 20
```
**Expected (chronological):** #6(2008) → #7(2010) → #8(2011) → #5(2012) → #4(2013) → #10(2013)

---

### Q21. What are the most recent documents in the collection?
```vql
SEARCH "library manuscript discovery characters"
USING EMBEDDINGS
WHERE FechaDoc >= "2016-01-01"
OPTIMIZE REORDER CHRONOLOGICAL
RETURN TEXT, METADATA
LIMIT 10
```
**Expected:** #3(2016-04) → #9(2016-11) → #1(2017-02) → #0(2018-01)

---

### Q22. Show events related to Carmen Vega ordered chronologically
```vql
SEARCH "Carmen Vega story events relationship secret"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
OPTIMIZE REORDER CHRONOLOGICAL
RETURN TEXT, METADATA
LIMIT 10
```
**Expected:** Nodes #1(2017-02) and #2(2000-10) — Carmen's friendship with Don Ricardo and her death

---

## TYPE 7 — PROCEDURAL / INSTRUCTIONAL
> Strategy: EMBEDDINGS · RERANK · REORDER CHRONOLOGICAL · moderate top-k

### Q23. What steps did Elena and Tomás follow to find the manuscript?
```vql
SEARCH "steps process search investigation find manuscript how they did it"
USING EMBEDDINGS
RERANK "sequence of steps to find the document"
THRESHOLD GLOBAL 0.65
OPTIMIZE REORDER CHRONOLOGICAL
RETURN TEXT, HIGHLIGHTS
LIMIT 8
```
**Expected sequence:** Tomás visits (#1) → they collaborate (#3) → find Martín (#5) → secret revealed (#6) → find manuscript (#7)

---

### Q24. How was the manuscript preserved and shared with the public?
```vql
SEARCH "how manuscript preserved digitized shared public exhibition"
USING EMBEDDINGS
RERANK "steps to preserve and digitize the manuscript"
THRESHOLD GLOBAL 0.65
RETURN TEXT, HIGHLIGHTS
LIMIT 5
```
**Expected:** Pablo digitizes (#8) → Ana creates exhibition (#9) → library becomes research center → mayor funds restoration (#10)

---

## TYPE 8 — MULTI-HOP / CHAINED REASONING
> Strategy: Decompose into sub-questions · one HYBRID query per hop · result feeds next WHERE/SEARCH

### Q25. What is the profession of Elena's cousin's husband?
```vql
-- HOP 1: Who is Elena's cousin?
SEARCH "Elena cousin family relation"
USING HYBRID WITH WEIGHTS (0.3, 0.7)
THRESHOLD LEXICAL 0.72
RETURN TEXT, SCORE
LIMIT 2
-- Result: Lucía Mendoza

-- HOP 2: Who is Lucía's husband?
SEARCH "Lucía Mendoza married husband"
USING HYBRID WITH WEIGHTS (0.3, 0.7)
THRESHOLD LEXICAL 0.70
RETURN TEXT, SCORE
LIMIT 2
-- Result: Dr. Fernando Ruiz

-- HOP 3: What is Fernando Ruiz's profession?
SEARCH "Dr. Fernando Ruiz profession occupation"
USING HYBRID WITH WEIGHTS (0.3, 0.7)
THRESHOLD LEXICAL 0.75
RETURN TEXT, SCORE
LIMIT 1
-- Result: Professor of archaeology at the National University
```
**Answer chain:** Elena → Lucía Mendoza → Dr. Fernando Ruiz → Professor of Archaeology

---

### Q26. Who funded the library restoration and how is he related to the investigation team?
```vql
-- HOP 1: Who funded the restoration?
SEARCH "who funded provided money restoration building library"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
THRESHOLD GLOBAL 0.72
RETURN TEXT, SCORE
LIMIT 2
-- Result: Mayor Roberto Jiménez

-- HOP 2: How is Jiménez connected to the team?
SEARCH "Roberto Jiménez relationship connection Dr. Ruiz team"
USING HYBRID WITH WEIGHTS (0.3, 0.7)
THRESHOLD LEXICAL 0.68
RETURN TEXT, SCORE
LIMIT 2
-- Result: Brother-in-law of Dr. Fernando Ruiz
```
**Answer chain:** Restoration funding → Roberto Jiménez → brother-in-law of Dr. Ruiz (Node #10)

---

### Q27. Who was the mentor of the man who entrusted the caretaker with the secret?
```vql
-- HOP 1: Who entrusted Martín with the secret?
SEARCH "who entrusted Martín López secret location manuscript"
USING HYBRID WITH WEIGHTS (0.3, 0.7)
THRESHOLD LEXICAL 0.72
RETURN TEXT
LIMIT 2
-- Result: Don Ricardo Morales

-- HOP 2: Who was Don Ricardo's mentor?
SEARCH "Don Ricardo mentor teacher spiritual guide"
USING HYBRID WITH WEIGHTS (0.4, 0.6)
RERANK "mentor name of Don Ricardo"
THRESHOLD GLOBAL 0.75
RETURN TEXT
LIMIT 2
-- Result: Fray Antonio Domínguez
```
**Answer chain:** Martín's secret keeper → Don Ricardo → mentor: Fray Antonio Domínguez (Nodes #5, #6, #7)

---

## Summary table

| Q# | Type | Key challenge | Nodes involved |
|----|------|---------------|----------------|
| Q1 | Factual | Exact founder name | #0 |
| Q2 | Factual | Profession lookup | #3 |
| Q3 | Factual | Family relation | #4 |
| Q4 | Factual | Proper noun + relation | #9 |
| Q5 | Factual | Object identity | #7 |
| Q6 | Factual | Mentor relationship | #7 |
| Q7 | Factual | Numeric data | #5 |
| Q8 | Comparative | Two roles balanced | #0,#1,#3,#8 |
| Q9 | Comparative | Contribution comparison | #3,#4 |
| Q10 | Comparative | Key vs supporting role | #5,#6,#8,#9 |
| Q11 | Analytical | Condition for revelation | #5,#6 |
| Q12 | Analytical | Causal chain | #8,#9,#10 |
| Q13 | Analytical | Family ties as mechanism | #0,#1,#4,#6,#10 |
| Q14 | Exploratory | All characters overview | all |
| Q15 | Exploratory | Theme extraction | all |
| Q16 | Exploratory | Library history | #0,#5,#6,#7,#10 |
| Q17 | Aggregation | Complete character list | all |
| Q18 | Aggregation | All professions | #0,#3,#4,#5,#8,#9,#10 |
| Q19 | Aggregation | All family links | #0,#1,#4,#8,#10 |
| Q20 | Temporal | Before 2015 chronological | #4,#5,#6,#7,#8,#10 |
| Q21 | Temporal | Most recent docs | #0,#1,#3,#9 |
| Q22 | Temporal | Carmen Vega timeline | #1,#2 |
| Q23 | Procedural | Discovery sequence | #1,#3,#5,#6,#7 |
| Q24 | Procedural | Preservation sequence | #8,#9,#10 |
| Q25 | Multi-hop | 3-hop: cousin→husband→job | #4,#3 |
| Q26 | Multi-hop | 2-hop: funder→relation | #10 |
| Q27 | Multi-hop | 2-hop: secret→keeper→mentor | #5,#6,#7 |
