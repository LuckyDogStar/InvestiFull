'''
Programmer: Brian Peters

Model Module that handles databasing. This is done with sqlite3 and supports creating Tables
that are meant to store information on fullerene's topologies.
'''
import sqlite3

class DataBase:
    #Pre: None
    #Post: instance of DataBase is created. SQLite connection established
    #Narrative: Constructor to create the instance of the DataBase aswell as establishing
        #A SQLite connection and setting the current table to InvalidTable
    def __init__(self, name):
        self.conn = sqlite3.connect(name)
        self.currtable = "InvalidTable"

    #Pre: Instance Exists
    #Post: connection to SQLite database closed
    #Narrative: Destructor to close off Database connection before deleting objective instance
    def __del__(self):
        self.conn.close()

    #Pre: Tables Exist
    #Post: Names of tables returned as strings in a python list
    #Narrative: Get and return a list of all the table names
    def GetTables(self):
        db = self.conn.cursor()
        returnlist = []
        try:
            tables = db.execute("SELECT name FROM sqlite_master WHERE type='table'")

            #Since the sql operation gives me tuples with the second value being empty i want to get the strings of the name
            i = 0
            for table in tables:
                returnlist.append(table[0])
                i += 1
            return returnlist

        except sqlite3.OperationalError:
            print("SQLITE Error in checking for table existence UwU:", db)

    #Pre: Tables Exist
    #Post: Number of tables that exist in the database returned
    #Narrative: Checks the SQLite database to see how many tables exist.
    def TableCount(self):
        db = self.conn.cursor()
        db.execute("SELECT count(name) FROM sqlite_master WHERE type='table'")
        tablenum = db.fetchone()[0]
        return tablenum

    #Pre: currtable is set
    #Post: Name of the current table is returned as a string
    #Narrative: Get the name of the currently selected table
    def GetCurrTable(self):
        return str(self.currtable)

    #Pre: A table exists to change to
    #Post: The current table is switched to the table with the given name
    #Narrative: Changes the table to the table with the name given by the user. If the table
        #Doesn't exist the current table remains the same.
    def ChangeTable(self, name):
        tablelist = self.GetTables()
        if name in tablelist:
            self.currtable = name
        else:
            print("That table does not exist")

    #Pre: enough space exists to create the new table
    #Post: The new table is created
    #Narrative: Takes a name and attempts to create a new table with that name
    def NewTable(self, name):
        db = self.conn.cursor()
        if not self.CheckTable(name):
            db.execute("CREATE TABLE "+name+" (info text, isomer text)")
            self.conn.commit()
        else:
            print("SQLITE Error: Table already exists")

    ##Drops table of the given name
    def DropTable(self, name):
        db = self.conn.cursor()
        db.execute("DROP TABLE "+name)
        self.conn.commit()

    #Pre: records is passed as a list and contains the records for the DataBase
    #Post: Records are added to the table
    #Narrative: Add records to a table This takes a list of records which should be strings
        #The string should be the data from sprial.f, a comma, and then the isomer it came from
    def AddRecord(self, records, iso):
        db = self.conn.cursor()
        db.executemany("INSERT INTO " + self.currtable + " VALUES (?, ?)", records)
        self.conn.commit()

    #Pre: None
    #Post: the database checks for a table with the given name
    #Narrative: Bool function that checks if a database table exists or not
    def CheckTable(self, name):
        #Assume we dont have the table then check for it
        exists = False
        db = self.conn.cursor()

        try:
            db.execute("SELECT count(name) FROM sqlite_master WHERE type='table' AND name='" + name+"'")
            if(db.fetchone()[0] == 1):
                exists = True

        except sqlite3.OperationalError:
            print("SQLITE Error in checking for table existence UwU:", db)

        return exists

    #Pre: records exist
    #Post: Retrieve all of the records from the current table
    #Narrative: Print all of the records from the table as a list of strings
    def GetRecords(self):
        records = []
        for record in self.conn.cursor().execute("SELECT * FROM " + self.currtable):
            records.append(record)
        return records

    #accomplishes the same as the previos GetRecords() method but it returns the DataBase
        #cursor object instead of a list of reocrds
    def GetRawRecords(self):
        return self.conn.cursor().execute("SELECT * FROM " + self.currtable)

#############End of Class###############
