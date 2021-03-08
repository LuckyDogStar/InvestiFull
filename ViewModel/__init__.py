'''
Programmer: Brian Peters

This is the View Model. Investiful is modeled using the Model View ViewModel Model.
The objective of this module is to translate information from the model (the Data Base)
into a form usable by the view. This module will also take input from the view and
make the appropriate modifications/additions to the model. To do this the subprocess
module is used to utilize spiral.exe for fullerene information.
'''
from DB import DataBase
import subprocess

INVALIDTABLE = "Invalid Table"

class ViewModel:

    #Pre: nothing
    #Post: An instance of the ViewModel is created. The DataBase
        #is created with the name fuller.db
    #Narrative: Constructor
    def __init__(self):
        self.currtable = str(INVALIDTABLE)
        self.datb = DataBase("fuller.db")

    #Pre: No pre-conditions required
    #Post: All of the table names are returned in the form of a python list of strings
    #Narrative: This returns a list contianing all the names of the tables existing in the database
    def GetTableNames(self):
        tablenames = self.datb.GetTables()
        return tablenames

    #Pre: A valid string is passed in
    #Post: A boolean value is returned: true for table exists, false for non existant.
    #Narrative: This calls the CheckTable from the DataBase Module
    def CheckTable(self, name):
        return self.datb.CheckTable(name)

    #Pre: Database Exists
    #Post: A count of how many tables exist in the databas is returned
    #Narrative: This method returns an integer of how many tables are in the database
    def TableCount(self):
        num = self.datb.TableCount()
        return num

    #Pre: Other Tables exist to change to
    #Post: If a valid table name was given, it's changed to. If the name is invalid then the table is not changed.
    #Narrative: Changes the current table to a new table based on the name given.
        #Returns true on success False on failure
    def ChangeTable(self, name):
        names = self.GetTableNames()

        if name in names:
            self.currtable = name
            self.datb.ChangeTable(name)
            return True
        else:
            return False

    #Pre: Space exists on the machine for more Tables
    #Post: The table is created with the given name, given that the name provided was valid
        # see the README for what table names are invalid
    #Narrative: Adds a table with the given name to the database.
    def AddTable(self, name):
        #Make sure the name isn't already in the database
        if not(self.datb.CheckTable(name)) and name != "table":
            self.datb.NewTable(name)
            self.datb.ChangeTable(name)
            self.currtable = name
            return True
        return False

    #Pre: Name of an existing table is given
    #Post: The table with the given name is dropped
    #Narrative: Drops the table with the given name
        #Returns false if the table doesn't exist in the database.
        #If the current table is dropped, it is set to invalid
    def DropTable(self, name):
        names = self.GetTableNames()
        if name in names:
            if name == self.currtable:
                self.currtable = INVALIDTABLE
            self.datb.DropTable(name)
            return True
        return False

    #Pre: Records exist in the current table
    #Post: Records from the database are returned in the form of a python list
    #Narrative: returns a list of the records from the current table in the databse
    def GetRecords(self):
        if self.currtable != INVALIDTABLE:
            return self.datb.GetRecords()
        else:
            return []

    #This function acts similarly to the previos GetRecords but it instead Returns
    #The datbase cursor object from SQLite. This is in here to allow certain interfaces used
    #For creating a GUI to have access to a database object for extraction of records.
    def GetRawRecords(self):
        return self.datb.GetRawRecords()

    #Pre: Iso and Pentaconnect are within range. Iso is the Isomner given by the user
        # and pentaconnect is whether or not they want to allow touching pentagons
    #Post: the records are added to the database.
    #Narrative: Using subprocess sprial.exe is called to obtain the topological info and
        #the output from sprial.exe is thrown into a python list. This is added into the DataBase
        #using the database AddRecord() method
    def AddIso(self, iso, pentaconnect):
        #start setting up the input for the database
        records = []
        if not iso.isnumeric() or int(iso) > 200 or int(iso) < 0:
            return records

        dbInput = str(iso)

        #add on a space then the pentagon rule to format the input for spiral.f
        dbInput += " "
        if not pentaconnect.isnumeric() or int(pentaconnect) > 1 or int(pentaconnect) < 0:
            return records
        dbInput += pentaconnect

        #call spiral.f and have a pipe set up to get the input
        p = subprocess.Popen("./spiral.exe", stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        if not p:
            print("Error with opening")

        #read spiral.f output into x
        x = p.communicate(input=dbInput)[0]

        #create a list of all of the lines of output
        lines = x.split("\n")

        #get rid of the extra lines describing what the table is showing
        if len(lines) > 1:
            lines.pop(0)
            lines.pop(0)
            lines.pop(len(lines)-1)
            lines.pop(len(lines)-1)



            for line in lines:
                #strip off unnecessary pieces of string
                line = line.strip()
                #format the line for database input
                line = list(line)
                line[0] = " "
                line = "".join(line)
                line = line.strip()
                line = (iso, line)
                #add to list of records to be added to database
                records.append(line)
            self.datb.AddRecord(records, iso)

        return self.datb.GetRecords()

    #Method that calls AddIso multiple times for a given range
    def AddIsos(self, lowerBound, upperBound, pentaconnect):
        upperBound += 1
        for iso in range (lowerBound, upperBound):
            self.AddIso(str(iso), pentaconnect)
        return self.datb.GetRecords()

    #Pre: none
    #Post: the current table is returned as a string value
    #Narrative: Simple method to retrieve the name of the current table
    def GetCurrTable(self):
        return self.currtable
