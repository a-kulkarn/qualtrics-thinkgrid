# Thing that Zac wants to do.

- Take in the giant data file (CSV) which contains the text strings for the survey quesitons.

- (DONE) Love to say: (Based on aforementioned datafile. Create: ...)
  - For `row "x"`, then create a block named 
   ```
   ID_{cell value for row "x" under "id" field}
   ```
   Question text should be the text from the appropriate cell in `{row "x", column "thoughts"}`.
  - One question per block.
  
- (DONE) Fix surveyflow
- (DONE) If possible, fix the squares.

- Problem: Question options correspond to a unique column. 
    - Not a big deal. Just need to partition with a reasonable randomization strategy.
  
  
# Email tasks

## Before Study 1 goes live:

(DONE) 1. Fix survey flow (all of the blocks in randomizer)
(DONE) 2. Rename squares: replace "S1" with '<ID>_<Region #>'
(DONE) 3. Rename blocks: replace "if this matters someone will tell me
    eventually" with '<ID>_block'

## After Study 1 goes live:

4. (DONE) Add pull logic: If <column A, row X>=<variable 1> [or <variable 2>]
   and <column B, row X> = <variable 3> [or <variable 2>], then create
   a block for row X

5. (DONE) Add grid logic: Define boxes by three variables (1) <box side
   length>; (2) gap size; (3) origin point (bottom left corner of
   bottom left block)

6. (DONE) Create new study every 400 blocks

7. Test new functionality introduced above.

