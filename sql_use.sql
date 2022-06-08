-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT 1

CREATE TABLE student2 (
	student_id INT,
	name VARCHAR(50),
	major VARCHAR(100),
	PRIMARY KEY(student_id)
);

INSERT INTO student2 VALUES(1, 'Jack', 'Biology');
INSERT INTO student2 VALUES(2, 'Kate', 'Sociology');
INSERT INTO student2 VALUES(3, 'Claire', 'Chemistry');
INSERT INTO student2 VALUES(4, 'Jack', 'Biology');
INSERT INTO student2 VALUES(5, 'Mike', 'Computer Science');


