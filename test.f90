PROGRAM rolldice
  IMPLICIT NONE

  REAL                    ::  p1, percentage, temp
  integer                 ::  i, j, wincounter, result, rollbonus, inplayers
  integer                 ::  maxi=100000
  integer, allocatable    ::  players(:)

  wincounter = 0  ! tracks how many times p1 wins

  write(*,*) 'Select the rolling advantage!'
  read(*,'(I2)') rollbonus

  write(*,*) 'Select the amount of players!'
  read(*,'(I2)') inplayers
  allocate(players(inplayers))

  write(*,*)
  write(*,'(A,I2)') 'Player 1 has a rolling advantage of: ', rollbonus
  write(*,*)
  do i=1,maxi
    do j=1,inplayers
      CALL RANDOM_NUMBER(temp)
      players(j) = floor(temp*100) + 1
    end do
    players(1) = players(1) + rollbonus  ! bonus for player 1
    result = MAXVAL(players)

    if (players(1) == result) then
      wincounter = wincounter + 1
      ! write(*,*) 'The rolls:', players(:)
    end if
  end do
  percentage = wincounter*100./maxi
  write(*,'(A,f6.2,A)') 'Player 1 won', percentage, '% of the rolls'
  wincounter = 0

END PROGRAM rolldice
