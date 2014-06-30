#!/usr/bin/python

import queue
import sys
import threading
from threading import Event
from time import sleep

"""
Class that handles the connection attempts
to the selected IP adress  
"""

class connectToServer(threading.Thread):

    def __init__(self,serverSocket,ipAdress,delay,tries):
        """

        Initiate the class

        Key arguments:
        serverSocket - The socket to connect with
        ipAdress - The adress to connect to
        delay - How long the thread should wait untill trying again after a failed attempt
        tries - The remaining amount of attempts to reconnect
        """
        
        super(connectToServer,self).__init__()
        self.socket = serverSocket
        self.messageQueue = queue.Queue()
        self.ipAdress = ipAdress
        self.tries = int(tries)
        self.delay = float(delay)

#######################################################################################

    def run(self):

        """

        Runs the thread. Tries to connect to the IP adress. Puts "Connected", "Failed", or number of tries left
	in the queue depending on the outcome of the attempt
        """
        
        try:
            
            self.socket.connect((self.ipAdress,1337))
            self.messageQueue.put("Connected")
            self.tries = 0
        except Exception as e:
                self.tries -=1
                self.messageQueue.put(self.tries)
            
        if(self.tries > 1):
            sleep(self.delay)
            self.run()
        else:
            self.messageQueue.put("Failed")

#######################################################################################

    def returnQueue(self):

        """

        Returns the first element in the queue

        Return: "empty" if the queue is empty, else the head of the list
        """
        
        if self.messageQueue.empty():
            return "empty"
        else:
            return self.messageQueue.get()
