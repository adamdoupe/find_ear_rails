class HomeController < ApplicationController

  def search
    if params[:search].blank? or params[:section][:id].blank?
      redirect_to(:controller => "home")
    else
      @option = params[:section][:id]
      case @option
      when "1"
        redirect_to(:controller => "websites", :action => "index", :search => params[:search])
      when "2"
        redirect_to(:controller => "people", :action => "index", :search => params[:search])
      when "3"
        redirect_to(:controller => "jobs", :action => "index", :search => params[:search])
      when "4"
        redirect_to(:controller => "books", :action => "index", :search => params[:search])
      when "5"
        redirect_to(:controller => "conversations", :action => "index", :search => params[:search])
        default
        redirect_to(:controller => "home")
      end
    end
  end
end
