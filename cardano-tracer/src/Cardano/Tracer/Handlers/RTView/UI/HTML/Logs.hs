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
                  [ UI.table ## "logs-live-view-table" #. "table is-fullwidth rt-view-logs-live-view-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr #+
                              [ UI.th #. "rt-view-logs-live-view-node" #+
                                  [ string "Node"
                                  ]
                              , UI.th #. "rt-view-logs-live-view-timestamp" #+
                                  [ string "Timestamp"
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
                  ]
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-logs-live-view-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.div #. "field has-addons" #+
                          [ UI.p #. "control" #+
                              [ UI.div #. "dropdown is-hoverable" #+
                                  [ UI.anchor #. "navbar-link" #+ [element notificationsIcon]
                                  , UI.div #. "navbar-dropdown is-right" #+
                                      [ element notificationsEventsItem
                                      , element notificationsSettingsItem
                                      ]
                                  ]





                                <div class="dropdown is-hoverable">
                                  <div class="dropdown-trigger">
                                    <button class="button" aria-haspopup="true" aria-controls="dropdown-menu4">
                                      <span>Hover me</span>
                                      <span class="icon is-small">
                                        <i class="fas fa-angle-down" aria-hidden="true"></i>
                                      </span>
                                    </button>
                                  </div>
                                  <div class="dropdown-menu" id="dropdown-menu4" role="menu">
                                    <div class="dropdown-content">
                                      <div class="dropdown-item">
                                        <p>You can insert <strong>any type of content</strong> within the dropdown menu.</p>
                                      </div>
                                    </div>
                                  </div>
                                </div>













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
