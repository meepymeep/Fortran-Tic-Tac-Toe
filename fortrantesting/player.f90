MODULE player
 IMPLICIT NONE

 CHARACTER :: p_letter
 LOGICAL :: p_moving = .FALSE.

 CONTAINS ! Subroutines won't work without this
 ! for some reason.

 SUBROUTINE initialize_player(let)

  CHARACTER :: let

  p_letter = let

  IF (p_letter == 'x' .or. p_letter == 'X') THEN
   p_letter = 'X'
   p_moving = .TRUE.
  ELSE IF (p_letter == 'o' .or. p_letter == 'O') THEN
   p_letter = 'O'
  ELSE
   p_letter = 'X'
   p_moving = .TRUE.
  ENDIF

 END SUBROUTINE initialize_player
END MODULE player
