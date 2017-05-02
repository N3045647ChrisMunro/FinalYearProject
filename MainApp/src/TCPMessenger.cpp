//
//  TCPMessenger.cpp
//  MainApp - Final Year Project
//
//  Created by MUNRO, CHRISTOPHER on 01/05/2017.
//  Copyright © 2017 MUNRO, CHRISTOPHER. All rights reserved.
//

#include "TCPMessenger.h"

TCPMessenger::TCPMessenger()
{
}

TCPMessenger::~TCPMessenger()
{
}

//Add a message to the receive queue
void TCPMessenger::addMsgToRecvQueue(std::string msg)
{
    threadMutex_.lock();

    tcpMsgRecvQueue_.push(msg);

    threadMutex_.unlock();
}

//Get a message from the receive queue
std::string TCPMessenger::getRecvMessage()
{
    if (tcpMsgRecvQueue_.size() > 0) {

        threadMutex_.lock();

        std::string message = tcpMsgRecvQueue_.front();
        tcpMsgRecvQueue_.pop();

        threadMutex_.unlock();

        return message;
    }
    return nullptr;
}

//Add a message to the send queue
void TCPMessenger::addMsgToSendQueue(std::string msg)
{
    threadMutex_.lock();

    tcpMsgSendQueue_.push(msg);

    threadMutex_.unlock();
}

//Get a message form the send queue
std::string TCPMessenger::getSendMessage()
{
    if (tcpMsgSendQueue_.size() > 0) {
        threadMutex_.lock();

        std::string message;
        message = tcpMsgSendQueue_.front();
        tcpMsgSendQueue_.pop();

        //std::cout << "got from send " << std::endl;

        threadMutex_.unlock();

        return message;
    }
    return "";
}
