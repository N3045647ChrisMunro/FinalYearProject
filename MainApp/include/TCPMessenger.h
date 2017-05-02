//
//  TCPMessenger.h
//  MainApp - Final Year Project
//
//  Created by MUNRO, CHRISTOPHER on 01/05/2017.
//  Copyright � 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#pragma once

#include <queue>
#include <mutex>
#include <string>

class TCPMessenger
{
public:
	TCPMessenger();
	~TCPMessenger();

	void addMsgToSendQueue(std::string msg);
	std::string getSendMessage();

	void addMsgToRecvQueue(std::string msgString);
	std::string getRecvMessage();


private:

	std::mutex threadMutex_;

	std::queue<std::string> tcpMsgSendQueue_;
	std::queue<std::string> tcpMsgRecvQueue_;


};
