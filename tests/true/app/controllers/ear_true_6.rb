class PatientController < ApplicationController
  def create
    estimate = set_date() # check for estimated birthdates and alter params if necessary
        
    if  params[:patient_year] == "Unknown"
      flash[:error] = 'You must estimate patient age before saving!!'
      redirect_to :action =>"new"
      return
    end
    #temporary hack to facillitate patient registration
#     patient_day = "15"  if params[:patient_day].nil? || params[:patient_day]==""
    # return render_text patient_day
   
    patient_birthdate = params[:patient_day].to_s + "-" + params[:patient_month].to_s + "-" + params[:patient_year].to_s 
  #put validation to check if patient has id then @patient should be initialised to this
  #
    if params[:patient_id].nil? or params[:patient_id].empty?
      begin
        @patient = Patient.new(params[:patient]) 
        @patient.save
        #render_text @patient.to_yaml and return
        @patientname = PatientName.new(params[:patient_name])
        @patientname.patient = @patient
        unless @patientname.save           
         flash[:error] = 'Could not save patientname'
         redirect_to :action => 'error'
        end
      rescue StandardError => message
        flash[:error] = message.to_s.humanize
        redirect_to request.referer and return
      end
    else
      @patient = Patient.find(params[:patient_id])
    end    
#    @patient.birthdate = Date.new(params[:patient_year].to_i, params[:patient_month].to_i, params[:patient_day].to_i)

		@patient.birthdate = patient_birthdate.to_date.to_s 
		@patient.birthdate_estimated = estimate  
  
    @patientaddress = PatientAddress.new(params[:patientaddress])
    @patientaddress.patient=@patient
    unless @patientaddress.save           
      flash[:error] = 'Could not save patientaddress'
      redirect_to :action => 'error'
    end

    @current_ta = PatientIdentifier.new(params[:current_ta]);
    @current_ta.patient = @patient
    @current_ta.identifier_type = PatientIdentifierType.find_by_name("Traditional authority").patient_identifier_type_id
    unless @current_ta.save
      flash[:error] = 'Could not save current patient_ta'
      redirect_to :action => 'error'
    end
  
  
    @other_name = PatientIdentifier.new(params[:other_name])
    @other_name.patient = @patient
    @other_name.identifier_type = PatientIdentifierType.find_by_name("Other name").patient_identifier_type_id
    unless @other_name.save
      flash[:error] = 'Could not save patients other name'
      redirect_to :action => 'error'
    end
     
    ask_extra_phone_numbers=GlobalProperty.find_by_property("ask_multiple_phone_numbers").property_value
    if ask_extra_phone_numbers=="true" 
      if params[:cell_phone][:identifier] !=""
        @cell_phone = PatientIdentifier.new(params[:cell_phone])
        @cell_phone.patient = @patient
          unless params[:cell_phone][:identifier].nil? or params[:cell_phone][:identifier].empty?
            @cell_phone.identifier_type = PatientIdentifierType.find_by_name("Cell phone number").patient_identifier_type_id
          end  
          unless @cell_phone.save
            flash[:error] = 'Could not save patients phone number'
            redirect_to :action => 'error'
          end
      end
    
      if params[:office_phone][:identifier] !="" and  params[:office_phone][:identifier].nil?           
        @office_phone = PatientIdentifier.new(params[:office_phone])
        @office_phone.patient = @patient
        unless params[:office_phone][:identifier].nil? or params[:office_phone][:identifier].empty?
          @office_phone.identifier_type = PatientIdentifierType.find_by_name("Office phone number").patient_identifier_type_id
        end  
        unless @office_phone.save
           flash[:error] = 'Could not save patients phone number'
           redirect_to :action => 'error'
        end
      end
    end

    if params[:home_phone][:identifier] !=""
    @home_phone = PatientIdentifier.new(params[:home_phone])
    @home_phone.patient = @patient
    unless params[:home_phone][:identifier].nil? or params[:home_phone][:identifier].empty?
      @home_phone.identifier_type = PatientIdentifierType.find_by_name("Home phone number").patient_identifier_type_id
    end  
    unless @home_phone.save
        flash[:error] = 'Could not save patients phone number'
        redirect_to :action => 'error'
     end
    end                                                    

    @occupation = PatientIdentifier.new()
    @occupation.identifier = params[:occupation]

    @occupation.patient = @patient
    @occupation.identifier_type = PatientIdentifierType.find_by_name("Occupation").patient_identifier_type_id
    unless @occupation.save
      flash[:error] = 'Could not save patients Occupation'
      redirect_to :action => 'error'
    end
    
    @p_address = PatientIdentifier.new(params[:p_address])
    @p_address.patient = @patient
    @p_address.identifier_type = PatientIdentifierType.find_by_name("Physical address").patient_identifier_type_id
    unless @p_address.save
      flash[:error] = 'Could not save patients Physical address'
      redirect_to :action => 'error'
    end

    @patient.set_national_id # setting new national id
     
    @patient_or_guardian = session[:patient_id].nil? ? "patient" : "guardian"
    if @patient_or_guardian !="guardian" and User.current_user.activities.include?("HIV Reception") and  GlobalProperty.find_by_property("use_filing_numbers").property_value == "true"
      @patient.set_filing_number
    end
                             
    if @patient.save  
      flash[:info] = 'Patient was successfully created.'
      if GlobalProperty.find_by_property("use_filing_numbers").property_value == "true" and User.current_user.activities.include?("HIV Reception")
        archived_patient = @patient.patient_to_be_archived
        message = printing_message(@patient,archived_patient,creating_new_patient=true) unless archived_patient.blank?
        print_and_redirect("/label/filing_number_and_national_id/#{@patient.id}", "/patient/set_patient/#{@patient.id}",message,next_button=true,@patient.id) unless message.blank?
        print_and_redirect("/label/filing_number_and_national_id/#{@patient.id}", "/patient/set_patient/#{@patient.id}") if message.blank?
      else
        print_and_redirect("/label/national_id/#{@patient.id}", "/patient/set_patient/#{@patient.id}")
      end
    else
      redirect_to :action => 'new'
    end
  end
end
