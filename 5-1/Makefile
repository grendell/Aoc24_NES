AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

5-1.nes: $(OBJ) $(OBJ)/5-1.o
	$(LD) $(LD_FLAGS) $(OBJ)/5-1.o -o 5-1.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/5-1.o: 5-1.s ../inputs/5.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 5-1.s -o $(OBJ)/5-1.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 5-1.nes