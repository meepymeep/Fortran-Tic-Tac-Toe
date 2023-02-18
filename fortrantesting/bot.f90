MODULE bot
 use player
 IMPLICIT none

 ! Bot data
 CHARACTER :: b_letter
 LOGICAL :: b_moving = .FALSE.

 CONTAINS ! Subroutines down below
 SUBROUTINE initialize_bot
  IF (p_letter == 'X') THEN
   b_letter = 'O'
  ELSE IF (p_letter == 'O') THEN
   b_letter = 'X'
   b_moving = .TRUE.
  ENDIF
 END SUBROUTINE initialize_bot
END MODULE bot
