TARGET = kb

all: $(TARGET)

$(TARGET): sources.mlb
	mlton -output kb sources.mlb
clean:
	rm -f $(TARGET) 
