CREATE DATABASE inventory_system;
USE inventory_system;

CREATE TABLE `maker_list` (
  `id` int NOT NULL AUTO_INCREMENT,
  `Name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `Pinyin` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=202 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

CREATE TABLE `item_type_data` (
  `id` int NOT NULL AUTO_INCREMENT,
  `MajorType` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `MajorTypeSKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `MinorType` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `MinorTypeSKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=167 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

CREATE TABLE `inventory` (
  `SKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `Maker` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `MajorType` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `MinorType` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `ItemName` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `ProductCost` decimal(10,2) NOT NULL DEFAULT '0.00',
  `ShippingCost` decimal(10,2) NOT NULL DEFAULT '0.00',
  `Quantity` int NOT NULL DEFAULT '0',
  `DomesticQuantity` int NOT NULL DEFAULT '0',
  `TransitQuantity` int NOT NULL DEFAULT '0',
  `UsQuantity` int NOT NULL DEFAULT '0',
  `ItemImagePath` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`SKU`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

CREATE TABLE `unique_items` (
  `UniqueID` varchar(36) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `SKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `ProductCost` decimal(10,2) NOT NULL,
  `DomesticShippingCost` float NOT NULL DEFAULT '0',
  `Status` enum('采购','国内入库','国内出库','国内售出','美国入库','美国发货','美国调货','退货','完成') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `Defect` enum('未知','无瑕','瑕疵','修复') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `DefectNotes` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `PurchaseTime` date DEFAULT NULL,
  `DomesticEntryTime` date DEFAULT NULL,
  `DomesticExitTime` date DEFAULT NULL,
  `DomesticSoldTime` date DEFAULT NULL,
  `UsEntryTime` date DEFAULT NULL,
  `UsShippingTime` date DEFAULT NULL,
  `UsRelocationTime` date DEFAULT NULL,
  `ReturnTime` date DEFAULT NULL,
  `IntlShippingMethod` enum('海运','空运') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `IntlTracking` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `IntlShippingCost` float NOT NULL DEFAULT '0',
  `OrderID` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `PurchaseCheck` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`UniqueID`),
  KEY `SKU` (`SKU`),
  KEY `fk_orders_orderid` (`OrderID`),
  KEY `fk_intl_tracking` (`IntlTracking`),
  CONSTRAINT `fk_intl_tracking` FOREIGN KEY (`IntlTracking`) REFERENCES `intl_shipments` (`TrackingNumber`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `fk_orderid` FOREIGN KEY (`OrderID`) REFERENCES `orders` (`OrderID`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `unique_items_ibfk_1` FOREIGN KEY (`SKU`) REFERENCES `inventory` (`SKU`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

CREATE TABLE `orders` (
  `OrderID` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `UsTrackingNumber` varchar(50) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `CustomerName` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `CustomerNetName` varchar(50) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `Platform` enum('Etsy','Shopify','TikTok','其他') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `OrderImagePath` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `OrderNotes` text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  `OrderStatus` enum('备货','预定','调货','装箱','发出','在途','送达') COLLATE utf8mb4_unicode_ci NOT NULL,
  `LabelStatus` enum('无','已上传','已打印') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT '无',
  `created_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`OrderID`),
  KEY `idx_customer_name` (`CustomerName`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci 

CREATE TABLE `intl_shipments` (
  `TrackingNumber` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `ShippingMethod` enum('海运','空运') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `TotalCost` decimal(10,2) NOT NULL DEFAULT '0.00',
  `Status` enum('运单创建','包裹发出','在途运输','包裹送达') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '运单创建',
  `CreatedAt` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `UpdatedAt` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`TrackingNumber`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci 

CREATE TABLE `item_status_history` (
  `UniqueID` varchar(36) COLLATE utf8mb4_unicode_ci NOT NULL,
  `previous_status` enum('采购','国内入库','国内出库','国内售出','美国入库','美国发货','美国调货','退货','完成') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `previous_status_timestamp` timestamp NULL DEFAULT NULL,
  `change_time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`UniqueID`,`change_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci 

CREATE TABLE `transactions` (
  `TransactionID` varchar(12) COLLATE utf8mb4_unicode_ci NOT NULL,
  `AccountType` enum('工资卡','美元卡','买货卡','一般户卡') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `Amount` decimal(10,2) NOT NULL,
  `Balance` decimal(10,2) DEFAULT '0.00',
  `Remarks` text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  `TransactionImagePath` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `TransactionTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`TransactionID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci 

CREATE TABLE `requests` (
  `RequestID` varchar(36) COLLATE utf8mb4_unicode_ci NOT NULL,
  `SKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `Maker` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '制造商名称',
  `ItemImagePath` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '物品图片路径',
  `ItemDescription` text COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '物品描述',
  `Quantity` int NOT NULL COMMENT '采购数量',
  `RequestStatus` enum('待处理','已完成','紧急') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '待处理',
  `Remarks` text COLLATE utf8mb4_unicode_ci COMMENT '留言',
  `RequestType` enum('采购','出库') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '采购' COMMENT '请求类型',
  `CreatedAt` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `UpdatedAt` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`RequestID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

DELIMITER //

CREATE TRIGGER before_transaction_insert
BEFORE INSERT ON `transactions`
FOR EACH ROW
BEGIN
  DECLARE last_balance DECIMAL(10,2);

  -- 获取当前账户的最新余额
  SELECT Balance INTO last_balance
  FROM transactions
  WHERE AccountType = NEW.AccountType
  ORDER BY TransactionTime DESC
  LIMIT 1;

  -- 如果没有之前的记录，初始化余额为 0
  IF last_balance IS NULL THEN
    SET last_balance = 0.00;
  END IF;

  -- 仅当插入记录为最新记录时更新余额
  IF NEW.TransactionTime >= (SELECT MAX(TransactionTime) 
                             FROM transactions 
                             WHERE AccountType = NEW.AccountType) THEN
    SET NEW.Balance = last_balance + NEW.Amount;
  END IF;
END;
//

DELIMITER ;

