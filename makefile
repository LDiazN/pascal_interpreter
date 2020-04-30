

#Every file needed to compile the project
OBJS = Main.hs Pascal/Lexer.x Pascal/Parser.y Pascal/Data.hs Pascal/SymbolTable.hs Pascal/Analyzer.hs Pascal/Interpret.hs Pascal/Base.hs Pascal/Wrapper.hs
MAIN = Main.hs

OBJ_NAME = pascal

all: $(OBJS)
	alex Pascal/Lexer.x 
	happy Pascal/Parser.y
	ghc -package mtl --make $(MAIN) -o $(OBJ_NAME) 

clear:
	rm *.hi *.o
	rm Pascal/Parser.hs
	rm Pascal/Lexer.hs
	rm Pascal/*.hi Pascal/*.o
	rm pascal
