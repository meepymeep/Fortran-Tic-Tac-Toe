PROGRAM main
 use player
 use bot 

 IMPLICIT none
 
 ! Integers
 INTEGER :: iterator ! This variable is used for DO loops.
 INTEGER :: board_spot ! If you enter in the number "3" for this, it will go to:
                                              ! ⤓ this spot right here.
 ! Booleans
 LOGICAL :: running_program = .TRUE.

 ! Character arrays                     1   2   3   4   5   6   7   8   9
 CHARACTER(1), DIMENSION(9) :: board = ['', '', '', '', '', '', '', '', '']

 ! Characters
 CHARACTER :: chosen_let
 print *, "Choose your letter! (x, o)"
 read(*,*) chosen_let

 CALL initialize_player(chosen_let)
 CALL initialize_bot

 DO WHILE (running_program)

  CALL system("clear")
  CALL render_board

  CALL move

 END DO

 CONTAINS ! All the subroutines down below.
 SUBROUTINE render_board
  ! Pre-coding meep: I guarantee this will be painful considering
  ! some of the weird things printing in Fortran can do.

  ! Row one
  print *, board(1), " | ", board(2), "| ", board(3)
  print *, "--|--|--"

  ! Row two
  print *, board(4), " | ", board(5), "| ", board(6)
  print *, "--|--|--"

  ! Last row
  print *, board(7), " | ", board(8), "| ", board(9)

  ! Post-coding meep: It actually wasn't surprisingly!
  ! It worked flawlessly when finished!
 END SUBROUTINE render_board


 SUBROUTINE win_detection
  ! Player wins
    !! Horizontal !!
  
  IF (board(1) == p_letter .and. board(2) == p_letter .and. board(3) == p_letter) THEN
   print *, "Player wins!"
   running_program = .FALSE.
  ELSE IF (board(4) == p_letter .and. board(5) == p_letter .and. board(6) == p_letter) THEN
   print *, "Player wins!"
   running_program = .FALSE.
  ELSE IF (board(7) == p_letter .and. board(8) == p_letter .and. board(9) == p_letter) THEN
   print *, "Player wins!"
   running_program = .FALSE.
  ENDIF

    !! Vertical !!

  IF (board(1) == p_letter .and. board(4) == p_letter .and. board(7) == p_letter) THEN
   print *, "Player wins!"
   running_program = .FALSE.
  ELSE IF (board(2) == p_letter .and. board(5) == p_letter .and. board(8) == p_letter) THEN
   print *, "Player wins!"
   running_program = .FALSE.
  ELSE IF (board(3) == p_letter .and. board(6) == p_letter .and. board(9) == p_letter) THEN
   print *, "Player wins!"
   running_program = .FALSE.
  ENDIF

    !! Diagonal !!
 
  IF (board(1) == p_letter .and. board(5) == p_letter .and. board(9) == p_letter) THEN
   print *, "Player wins!"
   running_program = .FALSE.
  ELSE IF (board(3) == p_letter .and. board(5) == p_letter .and. board(7) == p_letter) THEN
   print *, "Player wins!"
   running_program = .FALSE.
  ENDIF

  ! Bot wins !
    !! Horizontal !!
  IF (board(1) == b_letter .and. board(2) == b_letter .and. board(3) == b_letter) THEN
   print *, "Player2 wins!"
   running_program = .FALSE.
  ELSE IF (board(4) == b_letter .and. board(5) == b_letter .and. board(6) == b_letter) THEN
   print *, "Player2 wins!"
   running_program = .FALSE.
  ELSE IF (board(7) == b_letter .and. board(8) == b_letter .and. board(9) == b_letter) THEN
   print *, "Player2 wins!"
   running_program = .FALSE.
  ENDIF

    !! Vertical !!

  IF (board(1) == b_letter .and. board(4) == b_letter .and. board(7) == b_letter) THEN
   print *, "Player2 wins!"
   running_program = .FALSE.
  ELSE IF (board(2) == b_letter .and. board(5) == b_letter .and. board(8) == b_letter) THEN
   print *, "Player2 wins!"
   running_program = .FALSE.
  ELSE IF (board(3) == b_letter .and. board(6) == b_letter .and. board(9) == b_letter) THEN
   print *, "Player2 wins!"
   running_program = .FALSE.
  ENDIF

    !! Diagonal !!
 
  IF (board(1) == b_letter .and. board(5) == b_letter .and. board(9) == b_letter) THEN
   print *, "Player2 wins!"
   running_program = .FALSE.
  ELSE IF (board(3) == b_letter .and. board(5) == b_letter .and. board(7) == b_letter) THEN
   print *, "Player2 wins!"
   running_program = .FALSE.
  ENDIF

 END SUBROUTINE win_detection
 

 SUBROUTINE move

  CALL win_detection

  IF (p_moving) THEN
   print *, "Player, choose your spot: "
   read(*,*) board_spot

   IF (board(board_spot) == p_letter .or. board(board_spot) == b_letter) THEN
    DO WHILE (board(board_spot) == p_letter .or. board(board_spot) == b_letter)
     read(*,*) board_spot
     IF (board(board_spot) /= p_letter .or. board(board_spot) /= b_letter) THEN
      board(board_spot) = p_letter

      p_moving = .FALSE.
      b_moving = .TRUE.

      EXIT
     ENDIF
    END DO
   ELSE
    board(board_spot) = p_letter

    p_moving = .FALSE.
    b_moving = .TRUE.
   ENDIF

  ELSE IF (b_moving) THEN
   print *, "Player2, choose your spot: "
   read(*,*) board_spot

   IF (board(board_spot) == p_letter .or. board(board_spot) == b_letter) THEN
    DO WHILE (board(board_spot) == p_letter .or. board(board_spot) == b_letter)
     read(*,*) board_spot
     IF (board(board_spot) /= p_letter .or. board(board_spot) /= b_letter) THEN
      board(board_spot) = b_letter

      b_moving = .FALSE.
      p_moving = .TRUE.
     ENDIF
    END DO
   ELSE
    board(board_spot) = b_letter

    p_moving = .TRUE.
    b_moving = .FALSE.
   ENDIF
  ENDIF

 END SUBROUTINE move

END PROGRAM main
