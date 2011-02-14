class Admin::EmailBlastsController < Admin::BaseController

  def index
  end

  def blast
    email_options = mailer_options.merge({:subject => params[:subject], :body => params[:body]})
    build_recipient_list(params[:to]).each do |user|
      AdminMailer.deliver_blast(user,email_options)
    end
    redirect_to :action => 'index'
  end
  
  protected

  def check_token_category(token_string, special)
    token_string =~ /#{special}/
  end
end

