package controller

import skinny._

class RootController extends ApplicationController {

  def index = {
    render("/root/index")
  }

  def scaml = {
    render("/root/scaml")
  }
}