{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Logs
  ( mkLogsLiveView
  ) where

import           Control.Monad (void)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkLogsLiveView :: UI Element
mkLogsLiveView = do
  closeIt <- UI.button #. "delete"

  searchMessagesInput <- UI.input #. "input rt-view-search-messages"
                                  # set UI.type_ "text"
                                  # set (UI.attr "placeholder") "Search log items"
  searchMessages <- UI.button #. "button is-info"
                              #+ [image "rt-view-search-logs-icon" searchSVG]

  whenToSearch <-
    UI.div #. "dropdown is-hoverable" #+
      [ UI.div #. "dropdown-trigger" #+
          [ UI.button #. "button"
                      # set ariaHasPopup "true"
                      # set ariaControls "dropdown-menu4" #+
              [ UI.span # set text "Last 3 days"
              -- ICON?
              ]
          ]
      , UI.div ## "dropdown-menu4"
               #. "dropdown-menu"
               # set role "menu" #+
          [ UI.div #. "dropdown-content" #+
              [ UI.div #. "dropdown-item" #+
                  [ UI.p # set text "TEST ME"
                  ]
              ]
          ]
      ]

  whereToSearch <-
    UI.div #. "dropdown is-hoverable" #+
      [ UI.div #. "dropdown-trigger" #+
          [ UI.button #. "button"
                      # set ariaHasPopup "true"
                      # set ariaControls "dropdown-menu4" #+
              [ UI.span # set text "In message"
              -- ICON?
              ]
          ]
      , UI.div ## "dropdown-menu4"
               #. "dropdown-menu"
               # set role "menu" #+
          [ UI.div #. "dropdown-content" #+
              [ UI.div #. "dropdown-item" #+
                  [ UI.p # set text "TEST ME"
                  ]
              ]
          ]
      ]

  logsLiveViewTable <-
    UI.div ## "logs-live-view-modal-window" #. "modal" # set dataState "closed" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-logs-live-view-modal" #+
          [ UI.header #. "modal-card-head rt-view-logs-live-view-head" #+
              [ UI.p #. "modal-card-title rt-view-logs-live-view-title" #+
                  [ string "Log items from connected nodes"
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-logs-live-view-body" #+
              [ UI.div #. "bd-notification" #+
                  [ UI.div ## "logs-live-view-nodes-checkboxes" #. "field" #+ []
                  ]
              , UI.div ## "logs-live-view-table-container" #. "table-container" #+
                  [ UI.div ## "node-logs-live-view-tbody"
                           # set dataState "0"
                           #+ []
                    {-
                    UI.table ## "logs-live-view-table" #. "table is-fullwidth rt-view-logs-live-view-table" #+
                      [ UI.mkElement "thead" # hideIt #+
                          [ UI.tr #+
                              [ UI.th #. "rt-view-logs-live-view-timestamp" #+
                                  [ string "Timestamp"
                                  ]
                              , UI.th #. "rt-view-logs-live-view-node" #+
                                  [ string "Node"
                                  ]
                              , UI.th #. "rt-view-logs-live-view-severity" #+
                                  [ string "Severity"
                                  ]
                              , UI.th #. "rt-view-logs-live-view-namespace" #+
                                  [ string "Namespace"
                                  ]
                              , UI.th #+
                                  [ string "Message"
                                  ]
                              , UI.th #. "rt-view-logs-live-view-copies" #+
                                  [ UI.span # set html "&nbsp;"
                                  ]
                              ]
                          ]
                      , UI.mkElement "tbody" ## "node-logs-live-view-tbody"
                                             # set dataState "0"
                                             #+ []
                      ]
                    -}
                  ]
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-logs-live-view-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.div #. "field has-addons" #+
                          [ UI.p #. "control" #+
                              [ element whenToSearch
                              ]
                          , UI.p #. "control" #+
                              [ element whereToSearch
                              ]
                          , UI.p #. "control" #+
                              [ element searchMessagesInput
                              ]
                          , UI.p #. "control" #+
                              [ element searchMessages
                              ]
                          ]
                      ]
                  --, UI.div #. "column has-text-right" #+
                  --    [ element exportToJSON
                  --    ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ do
    void $ element logsLiveViewTable #. "modal"
    void $ element logsLiveViewTable # set dataState "closed"

  return logsLiveViewTable
