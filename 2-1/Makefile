AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

2-1.nes: $(OBJ) $(OBJ)/2-1.o
	$(LD) $(LD_FLAGS) $(OBJ)/2-1.o -o 2-1.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/2-1.o: 2-1.s ../inputs/2.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 2-1.s -o $(OBJ)/2-1.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 2-1.nes