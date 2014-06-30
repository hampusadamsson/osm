import TabUtils
import ConnectionUtils
from Menu import Menues
from tkinter import *

def checkConnectQueue(gui,thread):
        """

        Parses the result from a connection attempt. 

        Keyword arguments:
        thread - The thread attempting to connect.

        """
        result = thread.returnQueue()
        if (result == "empty"):
            gui.master.after(500,checkConnectQueue,gui,thread)
            
        elif (result == "Connected"):
            gui.message.config(state=NORMAL)
            gui.socketStatus = "ok"
            TabUtils.writeMessage(gui,"You are now connected to " + gui.configList["ipAdress"] + "!")
            gui.Start()
            gui.menues = Menues(gui)
            ConnectionUtils.sendUserName(gui)
            ConnectionUtils.timeout_update(gui)
            gui.message.delete(0,END)
            if gui.configList["restoreTabs"] == "auto" and len(gui.windowList) > 1:
                TabUtils.restoreTabs(gui)
            else:
                TabUtils.deleteAllTabs(gui)
                TabUtils.clearWindowList(gui.windowList)
                
        elif (result == "Failed"):
            TabUtils.writeMessage(gui,"Connection failed, use /connect IP to try again")
            gui.message.config(state=NORMAL)
            gui.message.delete(0,END)
            
        else:
            TabUtils.writeMessage(gui,"No response from server. New attempt in " + gui.configList["delay"] + " seconds. " + str(result) + " attempts left")
            gui.master.after(2000,checkConnectQueue,gui,thread)
