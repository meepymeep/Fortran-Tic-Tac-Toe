compile:
	gfortran -c player.f90
	gfortran -c bot.f90
	gfortran -c main.f90
	gfortran -o main main.o player.o bot.o
	./main
clean:
	$(RM) main
	$(RM) main.o
	$(RM) player.o
	$(RM) player.mod
	$(RM) bot.o
	$(RM) bot.mod
