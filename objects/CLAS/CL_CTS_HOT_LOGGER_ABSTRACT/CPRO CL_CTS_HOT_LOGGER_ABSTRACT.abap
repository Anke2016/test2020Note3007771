  protected section.
    data:
      mt_messages type if_cts_hot_logger=>ty_t_sprot_u,
      "! Language of messages. Default 'E' so that always english messages are part of persistence.
      "! Log viewers will use logon language and translate texts based on msg_nr to correct language.
      mv_langu    type sprot_u-langu value 'E'.
