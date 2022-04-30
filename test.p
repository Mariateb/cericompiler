[a,b]

FOR a := 1 TO 10 DO
BEGIN
	b := 1;
	WHILE b < a DO
	BEGIN
		IF a%b == 0 THEN
		BEGIN
			DISPLAY b
		END;
		b := b + 1
	END
END.
