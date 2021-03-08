********************************************************
    _____                     _   _ ______     _ _ 
   |_   _|                   | | (_)  ____|   | | |
     | |  _ ____   _____  ___| |_ _| |__ _   _| | |
     | | | '_ \ \ / / _ \/ __| __| |  __| | | | | |
    _| |_| | | \ V /  __/\__ \ |_| | |  | |_| | | |
   |_____|_| |_|\_/ \___||___/\__|_|_|   \__,_|_|_|
 
 
	Bryan Peters	Dylan Whitehead		Austin Oakes
	
			Ryan Driver		Anthony Criscione
 
********************************************************


****	SOFTWARE DESCRIPTION	****


InvestiFull is software that provides topological 
information on fullerenes that exist within a given 
range of up to two hundred carbon atoms. InvestiFull 
is a python program that interfaces with an SQLite 
database, and the Fortran program ‘Spiral.f’ to 
produce topological information on the fullerenes

We did not develop spiral.f. It was retrieved from a 
github repository that has since been deleted. It is a 
popular fortran program used to produce topological information
on fullerene molecules. 

****		  TO SETUP	        ****


-Install Python
	
To install python:
	Ubuntu: sudo apt install python3
	Centos: yum install python3
	Debian: sudo apt-get install python3


****	     USING INVESTIFULL		****
To run InvestiFull, type the following command:
	
	python3 Investifull.py
	
This only requires python3 to run.

****	     EXAMPLE OF MAIN MENUE	****

 ------------------------
| Current Table: Invalid Table |
 ------------------------
What would you like to do?: 
********************* 
AddVal(V) 
CheckTables(C) 
AddTable(A) 
ChangeTable(T) 
PrintRecords(P) 
DropTable(D) 
Quit(Q) 


********************************************************

# DEFAULT #

InvestiFull by default has no tables.
Tables can be created and records can be
added to them


# ADDVAL #
	
The Add feature allows the user to
append to the current table contents
based on a specified number of carbon
atoms or a range. The carbon atom input 
should be between 1 and 200 inclusive. 
This is due to spiral.f functionality 
restrictions.
	
This will provide the appropriate
topolocial information on the isomer.



# CHECKTABLES #

Prints out a listing of all of the tables that
exist in the database.

	
# ADDTABLE #
			
This allows the user to add a new table to
the database. The only names not accepted
are "Table", "InvalidTable" or any name
containing symbols or punctuation. Numbers
are fine.
	

# CHANGETABLE #

Shows a listing of the tables that exist
and allows the user to input a name to 
change to a new table.
	
# PRINTRECORDS #

Print prints out a listing of all of the
records from the current table.


# DROPTABLE #

Using the delete feature will drop any
table specified by the user from the DB.
If table does not exist, nothing happens.
	
********************************************************









