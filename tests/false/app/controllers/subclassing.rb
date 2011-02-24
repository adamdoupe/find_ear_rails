# -*- coding: utf-8 -*-

class SubclassController < ApplicationController

  def redirect_to(*args)
    track(:redirecting => true)
    super
  end

  def create_and_accept_friendship
    if redirect_to "/cuenta" then
      return
    else
      return
    end
    freq.accept_external(@u)
  end

  protected
  def redirto_or(alt)
    redirect_to(params[:redirto] ? params[:redirto] : alt)
  end
  

end
