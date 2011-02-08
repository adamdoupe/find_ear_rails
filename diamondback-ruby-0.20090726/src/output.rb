class PunchesController < ApplicationController
  def index()
    __tmp_1 = Punch.all()
    __tmp_3 = params()
    __tmp_2 = __tmp_3.[](:"page")
    __tmp_4 = Punch.per_page()
    @punches = __tmp_1.paginate({:"page" => __tmp_2, :"per_page" => __tmp_4})
    @punch = Punch.new()
    __tmp_8 = respond_to() {|format|
      format.html()
      format.xml() {|*__tmp_5|
        __tmp_6 = render({:"xml" => @punches})
        next __tmp_6 
      }
      __tmp_7 = format.js()
      next __tmp_7 
    }
    return __tmp_8
  end
  def show()
    __tmp_10 = params()
    __tmp_9 = __tmp_10.[](:"id")
    @punch = Punch.find(__tmp_9)
    __tmp_14 = respond_to() {|format|
      format.html()
      __tmp_13 = format.xml() {|*__tmp_11|
        __tmp_12 = render({:"xml" => @punch})
        next __tmp_12 
      }
      next __tmp_13 
    }
    return __tmp_14
  end
  def new()
    @punch = Punch.new()
    __tmp_18 = respond_to() {|format|
      format.html()
      __tmp_17 = format.xml() {|*__tmp_15|
        __tmp_16 = render({:"xml" => @punch})
        next __tmp_16 
      }
      next __tmp_17 
    }
    return __tmp_18
  end
  def edit()
    __tmp_20 = params()
    __tmp_19 = __tmp_20.[](:"id")
    @punch = Punch.find(__tmp_19)
    return @punch
  end
  def create()
    __tmp_22 = params()
    __tmp_21 = __tmp_22.[](:"punch")
    @punch = Punch.new(__tmp_21)
    __tmp_36 = respond_to() {|format|
      __tmp_23 = @punch.parse_and_save()
      if __tmp_23 then
        format.html() {|*__tmp_24|
          __tmp_25 = punches_path()
          __tmp_26 = redirect_to(__tmp_25,
                                 {:"notice" => %{Punch was successfully created.}})
          next __tmp_26 
        }
        __tmp_34 = format.xml() {|*__tmp_27|
          __tmp_28 = render({:"xml" => @punch, :"status" => :"created",
                             :"location" => @punch})
          next __tmp_28 
        }
        next __tmp_34 
      else
        format.html() {|*__tmp_29|
          __tmp_30 = render({:"action" => %{new}})
          next __tmp_30 
        }
        __tmp_35 = format.xml() {|*__tmp_31|
          __tmp_32 = @punch.errors()
          __tmp_33 = render({:"xml" => __tmp_32,
                             :"status" => :"unprocessable_entity"})
          next __tmp_33 
        }
        next __tmp_35 
      end
    }
    return __tmp_36
  end
  def update()
    __tmp_38 = params()
    __tmp_37 = __tmp_38.[](:"id")
    @punch = Punch.find(__tmp_37)
    __tmp_40 = params()
    __tmp_39 = __tmp_40.[](:"punch")
    @punch.attributes=(__tmp_39)
    __tmp_54 = respond_to() {|format|
      __tmp_41 = @punch.parse_and_save()
      if __tmp_41 then
        format.html() {|*__tmp_42|
          __tmp_43 = punches_path()
          __tmp_44 = redirect_to(__tmp_43,
                                 {:"notice" => %{Punch was successfully updated.}})
          next __tmp_44 
        }
        __tmp_52 = format.xml() {|*__tmp_45|
          __tmp_46 = head(:"ok")
          next __tmp_46 
        }
        next __tmp_52 
      else
        format.html() {|*__tmp_47|
          __tmp_48 = render({:"action" => %{edit}})
          next __tmp_48 
        }
        __tmp_53 = format.xml() {|*__tmp_49|
          __tmp_50 = @punch.errors()
          __tmp_51 = render({:"xml" => __tmp_50,
                             :"status" => :"unprocessable_entity"})
          next __tmp_51 
        }
        next __tmp_53 
      end
    }
    return __tmp_54
  end
  def destroy()
    __tmp_56 = params()
    __tmp_55 = __tmp_56.[](:"id")
    @punch = Punch.find(__tmp_55)
    @punch.destroy()
    __tmp_63 = respond_to() {|format|
      format.html() {|*__tmp_57|
        __tmp_58 = punches_url()
        __tmp_59 = redirect_to(__tmp_58)
        next __tmp_59 
      }
      __tmp_62 = format.xml() {|*__tmp_60|
        __tmp_61 = head(:"ok")
        next __tmp_61 
      }
      next __tmp_62 
    }
    return __tmp_63
  end
  def autocomplete()
    @tags = nil
    __tmp_66 = params()
    __tmp_65 = __tmp_66.[](:"term")
    __tmp_64 = __tmp_65.blank?()
    if __tmp_64 then
      nil
    else
      __tmp_69 = params()
      __tmp_68 = __tmp_69.[](:"term")
      __tmp_67 = __tmp_68.first()
      case __tmp_67
      when %{\#} then
        __tmp_71 = params()
        __tmp_70 = __tmp_71.[](:"term")
        term = __tmp_70.gsub(%{\#}, %{})
        __tmp_73 = Punch.project_counts()
        __tmp_74 = term.to_s()
        __tmp_75 = %{%}.+(__tmp_74)
        __tmp_76 = __tmp_75.+(%{%})
        __tmp_72 = __tmp_73.where(%{tags.name like ?}, __tmp_76)
        @tags = __tmp_72.collect!() {|p|
          __tmp_78 = p.name()
          __tmp_77 = %{\#}.+(__tmp_78)
          next __tmp_77 
        }
      when %{@} then
        __tmp_80 = params()
        __tmp_79 = __tmp_80.[](:"term")
        term = __tmp_79.gsub(%{@}, %{})
        __tmp_82 = Punch.client_counts()
        __tmp_83 = term.to_s()
        __tmp_84 = %{%}.+(__tmp_83)
        __tmp_85 = __tmp_84.+(%{%})
        __tmp_81 = __tmp_82.where(%{tags.name like ?}, __tmp_85)
        @tags = __tmp_81.collect!() {|c|
          __tmp_87 = c.name()
          __tmp_86 = %{@}.+(__tmp_87)
          next __tmp_86 
        }
      when %{*} then
        __tmp_89 = params()
        __tmp_88 = __tmp_89.[](:"term")
        term = __tmp_88.gsub(%{*}, %{})
        __tmp_91 = Punch.action_counts()
        __tmp_92 = term.to_s()
        __tmp_93 = %{%}.+(__tmp_92)
        __tmp_94 = __tmp_93.+(%{%})
        __tmp_90 = __tmp_91.where(%{tags.name like ?}, __tmp_94)
        @tags = __tmp_90.collect!() {|a|
          __tmp_96 = a.name()
          __tmp_95 = %{*}.+(__tmp_96)
          next __tmp_95 
        }
      else
        __tmp_97 = params()
        term = __tmp_97.[](:"term")
        __tmp_99 = Punch.project_counts()
        __tmp_100 = term.to_s()
        __tmp_101 = %{%}.+(__tmp_100)
        __tmp_102 = __tmp_101.+(%{%})
        __tmp_98 = __tmp_99.where(%{tags.name like ?}, __tmp_102)
        @tags = __tmp_98.collect!() {|p|
          __tmp_104 = p.name()
          __tmp_103 = %{\#}.+(__tmp_104)
          next __tmp_103 
        }
        __tmp_108 = Punch.action_counts()
        __tmp_109 = term.to_s()
        __tmp_110 = %{%}.+(__tmp_109)
        __tmp_111 = __tmp_110.+(%{%})
        __tmp_107 = __tmp_108.where(%{tags.name like ?}, __tmp_111)
        __tmp_106 = __tmp_107.collect!() {|a|
          __tmp_113 = a.name()
          __tmp_112 = %{*}.+(__tmp_113)
          next __tmp_112 
        }
        __tmp_105 = @tags.<<(__tmp_106)
        __tmp_105
        __tmp_117 = Punch.client_counts()
        __tmp_118 = term.to_s()
        __tmp_119 = %{%}.+(__tmp_118)
        __tmp_120 = __tmp_119.+(%{%})
        __tmp_116 = __tmp_117.where(%{tags.name like ?}, __tmp_120)
        __tmp_115 = __tmp_116.collect!() {|c|
          __tmp_122 = c.name()
          __tmp_121 = %{@}.+(__tmp_122)
          next __tmp_121 
        }
        __tmp_114 = @tags.<<(__tmp_115)
        __tmp_114
      end
      @tags.flatten!()
    end
    __tmp_126 = respond_to() {|format|
      __tmp_125 = format.js() {|*__tmp_123|
        __tmp_124 = render({:"json" => @tags})
        next __tmp_124 
      }
      next __tmp_125 
    }
    return __tmp_126
  end
end
