import time
from tkinter import *
import copy

def messageSplit(message):
        """

        Reads to a blankspace and splits the string into two.

        Keyword arguments:
        input - The string to split.
        """
        index = message.find(" ")
        message2 = (message[0:index],message[index+1:len(message)])
        return message2

####################################################################################### 

def noDuplicate(windowList,name):
        """

        Checks if the window already exists

        Keyword arguments: 
        name - The name of the room.

        Return: 
        Zero if the name already exists else one. 
        """
        if name in windowList:
            return 0
        else:
            return 1

####################################################################################### 

def GetTime():
        """

        Import the current time and return it
        """
        return "<" + time.strftime("%H:%M")+" "

####################################################################################### 

def rename(gui):
        """

        Updates the configurationfile.
        """
        open('configFile', 'w').close()
        file = open('configFile','w')
        for element in gui.configList:
            file.write(element+"="+gui.configList[element]+'\n')
        file.close()
