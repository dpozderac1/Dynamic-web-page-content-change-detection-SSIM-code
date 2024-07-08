const puppeteer = require('puppeteer');
const fs = require('fs');
const path = require("path");

async function scrapeWebPages(currentPageUrl, pageFolderPath) {
    const browser = await puppeteer.launch({ headless: 'new' });
    const page = await browser.newPage();
    const cssPropertiesData = [];

    // web page loading
    await page.goto(currentPageUrl);

    // waiting for 1500ms for the web page to load
    await new Promise(r => setTimeout(r, 1500));

    // reshape the window based on the width and height of the web page currently being analyzed
    const pageHeight = await page.evaluate(() => document.documentElement.scrollHeight);
    const pageWidth = await page.evaluate(() => document.documentElement.scrollWidth);

    await page.setViewport({ width: pageWidth, height: pageHeight });

    const elements = await page.$$('body *');
    const elementCounter = {};

    for (const element of elements) {
        try {
            const isElementVisible = await page.evaluate((element1) => {
                const rect = element1.getBoundingClientRect();
                return (
                    rect.width > 10 &&
                    rect.height > 10 &&
                    getComputedStyle(element1).display !== 'none' &&
                    getComputedStyle(element1).visibility !== 'hidden'
                );
            }, element);

            if (await isElementLeaf(element) && isElementVisible) {
                trenutniTag = await getElementTagName(element);

                if (!(trenutniTag in elementCounter)) {
                    elementCounter[trenutniTag] = 0;
                } else {
                    elementCounter[trenutniTag] += 1;
                }

                let cssPropertiesObj = {};

                cssPropertiesObj = {
                    websiteUrl: currentPageUrl,
                    elementType: trenutniTag,
                    elementIndex: elementCounter[trenutniTag],
                    elementId: await getElementId(element),
                    elementClass: await getElementClassName(element),
                    cssProperties: await getElementStyles(element)
                };

                // perform screenshot in three different time intervals
                for (let i = 0; i < 3; i++) {
                    await element.screenshot({ path: `${pageFolderPath}/${trenutniTag}${elementCounter[trenutniTag]} screenshot br${i}.png` });
                    await new Promise(r => setTimeout(r, 1500));
                    // hover the element to make it change if it has some transition/animation effect
                    await element.hover();
                }
                cssPropertiesData.push(cssPropertiesObj);
            }
        }
        catch (error) {
            console.log(`Error is: ${error}`);
            continue;
        }
    }

    // write CSS properties to JSON file
    const jsonFileName = `${extractDomain(currentPageUrl)}_css_properties.json`;
    fs.writeFileSync(`${pageFolderPath}/${jsonFileName}`, JSON.stringify(cssPropertiesData));
    console.log(`CSS properties extracted and saved to ${jsonFileName}`);

    // close the browser
    await browser.close();
}

// function to determine tag of an element being analyzed
async function getElementTagName(elementHandle) {
    const tagName = await elementHandle.evaluate((element) => element.tagName);
    return tagName;
}

// function to determine id attribute of an element being analyzed
async function getElementId(elementHandle) {
    const id = await elementHandle.evaluate((element) => element.id);
    return id;
}

// function to determine class attribute of an element being analyzed
async function getElementClassName(elementHandle) {
    const className = await elementHandle.evaluate((element) => element.className);
    return className;
}

// function to check if the element is leaf in the DOM tree
async function isElementLeaf(elementHandle) {
    const isLeaf = await elementHandle.evaluate((element) => element.childElementCount === 0);
    return isLeaf;
}

// function for extracting style of element
async function getElementStyles(elementHandle) {
    const styles = await elementHandle.evaluate((element) => {
        const computedStyles = getComputedStyle(element);
        return Array.from(computedStyles).reduce((properties, property) => {
            properties[property] = computedStyles.getPropertyValue(property);
            return properties;
        }, {});
    });
    return styles;
}

// function for extracting domain from URL string
function extractDomain(url) {
    const regex = /^(?:https?:\/\/)?(?:[^@\n]+@)?(?:www\.)?([^:\/\n]+)/i;
    const match = url.match(regex);
    return match ? match[1] : null;
}

// SimilarWeb web pages, top 10 most visited
const webPageUrls = [
    // caetegory: Arts & Entertainment
    "https://youtube.com", "https://netflix.com", "https://bilibili.com", "https://max.com", "https://fandom.com", "https://imdb.com", "https://spotify.com", "https://miguvideo.com", "https://disneyplus.com", "https://pixiv.net",
    // caetegory: Business and Consumer Services
    "https://zillow.com", "https://usps.com", "https://canadapost-postescanada.ca", "https://ups.com", "https://fedex.com", "https://medium.com", "https://realtor.com", "https://narvar.com", "https://shopify.com", "https://redfin.com",
    // caetegory: Community and Society
    "https://dzen.ru", "https://jw.org", "https://biblegateway.com", "https://tinder.com", "https://badoo.com", "https://legacy.com", "https://churchofjesuschrist.org", "https://bible.com", "https://islamweb.net",
    // caetegory: Computers Electronics and Technology
    "https://google.com", "https://facebook.com", "https://instagram.com", "https://twitter.com", "https://baidu.com", "https://yandex.ru", "https://whatsapp.com", "https://tiktok.com", "https://live.com", "https://reddit.com",
    // caetegory: eCommerce and Shopping
    "https://amazon.com", "https://ebay.com", "https://amazon.co.jp", "https://rakuten.co.jp", "https://etsy.com", "https://amazon.de", "https://taobao.com", "https://aliexpress.com", "https://wildberries.ru", "https://ozon.ru",
    // caetegory: Finance
    "https://paypal.com", "https://chase.com", "https://tradingview.com", "https://investing.com", "https://caixa.gov.br", "https://intuit.com", "https://capitalone.com", "https://wellsfargo.com", "https://bankofamerica.com", "https://americanexpress.com",
    // caetegory: Food and Drink
    "https://trilltrill.jp", "https://tabelog.com", "https://cookpad.com", "https://allrecipes.com", "https://hotpepper.jp", "https://doordash.com", "https://giallozafferano.it", "https://kurashiru.com", "https://foodnetwork.com", "https://ubereats.com",
    // caetegory: Games
    "https://twitch.tv", "https://roblox.com", "https://chess.com", "https://nettruyenus.com", "https://douyu.com", "https://steampowered.com", "https://linkkf.app", "https://steamcommunity.com", "https://gamewith.jp", "https://ign.com",
    //caetegory: Health
    "https://nih.gov", "https://healthline.com", "https://mayoclinic.org", "https://webmd.com", "https://cvs.com", "https://medicalnewstoday.com", "https://clevelandclinic.org", "https://walgreens.com", "https://cdc.gov", "https://msdmanuals.com",
    // caetegory: Heavy Industry and Engineering
    "https://capcut.com", "https://myflixerx.to", "https://qcc.com", "https://indianoil.in", "https://procore.com", "https://edf.fr", "https://larchitetto.it", "https://grainger.com", "https://skyscrapercity.com", "https://tepco.co.jp",
]


async function mainScraping(currentURLs) {
    for (let i = 0; i < currentURLs.length; i++) {
        console.log(`Currently processing: ${i}) ${currentURLs[i]}`);

        // creating folder for the current web page
        const folderName = `${extractDomain(currentURLs[i])} screenshots and data`;
        const folderPath = path.join(__dirname, folderName);

        if (!fs.existsSync(folderPath)) {
            fs.mkdirSync(folderPath);
            await scrapeWebPages(currentURLs[i], folderPath);
        }
        else {
            console.log(`Folder ${folderName} already exists!`);
        }
    }
}

// calling the main function responsible for scraping web pages
mainScraping(webPageUrls);