class UpdatesController < ApplicationController
  
  def authorized?(action, resource)
    logged_in? &&
      case action
        when 'edit', 'update'
          redirect_to(tickets_path)
        when 'destroy'
          redirect_to(tickets_path)
        else
          true
      end
  end
end
