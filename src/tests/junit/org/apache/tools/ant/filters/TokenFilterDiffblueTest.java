package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.filters.TokenFilter.ChainableReaderFilter;
import org.apache.tools.ant.filters.TokenFilter.ContainsRegex;
import org.apache.tools.ant.filters.TokenFilter.ContainsString;
import org.apache.tools.ant.filters.TokenFilter.DeleteCharacters;
import org.apache.tools.ant.filters.TokenFilter.FileTokenizer;
import org.apache.tools.ant.filters.TokenFilter.IgnoreBlank;
import org.apache.tools.ant.filters.TokenFilter.ReplaceRegex;
import org.apache.tools.ant.filters.TokenFilter.ReplaceString;
import org.apache.tools.ant.filters.TokenFilter.StringTokenizer;
import org.apache.tools.ant.filters.TokenFilter.Trim;
import org.apache.tools.ant.util.LineTokenizer;
import org.apache.tools.ant.util.Tokenizer;
import org.junit.Test;

public class TokenFilterDiffblueTest {
  /**
   * Test ChainableReaderFilter {@link ChainableReaderFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link Native2AsciiFilter} (default constructor) ByLine is {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainableReaderFilter#chain(Reader)}
   */
  @Test
  public void testChainableReaderFilterChain_givenNative2AsciiFilterByLineIsFalse() throws IOException {
    // Arrange
    Native2AsciiFilter native2AsciiFilter = new Native2AsciiFilter();
    native2AsciiFilter.setByLine(false);

    // Act
    Reader actualChainResult = native2AsciiFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof TokenFilter);
    assertEquals("foo", ((TokenFilter) actualChainResult).readFully());
    assertNull(((TokenFilter) actualChainResult).getProject());
    assertFalse(((TokenFilter) actualChainResult).getInitialized());
    assertTrue(actualChainResult.ready());
  }

  /**
   * Test ChainableReaderFilter {@link ChainableReaderFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link Native2AsciiFilter} (default constructor).</li>
   *   <li>Then return {@link TokenFilter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainableReaderFilter#chain(Reader)}
   */
  @Test
  public void testChainableReaderFilterChain_givenNative2AsciiFilter_thenReturnTokenFilter() throws IOException {
    // Arrange
    Native2AsciiFilter native2AsciiFilter = new Native2AsciiFilter();

    // Act
    Reader actualChainResult = native2AsciiFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof TokenFilter);
    assertEquals("foo", ((TokenFilter) actualChainResult).readFully());
    assertNull(((TokenFilter) actualChainResult).getProject());
    assertFalse(((TokenFilter) actualChainResult).getInitialized());
    assertTrue(actualChainResult.ready());
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor) Flags is {@code g}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegexFlagsIsG_thenThrowBuildException() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setFlags("g");

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor) Flags is {@code i}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegexFlagsIsI_thenThrowBuildException() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setFlags("i");

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor) Flags is {@code i}.</li>
   *   <li>When {@code String}.</li>
   *   <li>Then return {@code String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegexFlagsIsI_whenString_thenReturnString() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setFlags("i");
    containsRegex.setPattern("i");

    // Act and Assert
    assertEquals("String", containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor) Flags is {@code m}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegexFlagsIsM_thenThrowBuildException() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setFlags("m");

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor) Flags is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegexFlagsIsNull_thenThrowBuildException() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setFlags(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor) Flags is {@code s}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegexFlagsIsS_thenThrowBuildException() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setFlags("s");

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor) Pattern is {@code i}.</li>
   *   <li>When {@code String}.</li>
   *   <li>Then return {@code String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegexPatternIsI_whenString_thenReturnString() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setPattern("i");

    // Act and Assert
    assertEquals("String", containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegexProjectIsProject_thenReturnString() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setProject(new Project());
    containsRegex.setPattern("i");

    // Act and Assert
    assertEquals("String", containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsRegex} (default constructor).</li>
   *   <li>When {@code String}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_givenContainsRegex_whenString_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ContainsRegex()).filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_thenReturnNull() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setPattern("jane.doe@example.org");

    // Act and Assert
    assertNull(containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex {@link ContainsRegex#filter(String)}.
   * <ul>
   *   <li>Then return {@code Stralice.liddell@example.orgng}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegex#filter(String)}
   */
  @Test
  public void testContainsRegexFilter_thenReturnStraliceLiddellExampleOrgng() {
    // Arrange
    ContainsRegex containsRegex = new ContainsRegex();
    containsRegex.setReplace("alice.liddell@example.org");
    containsRegex.setPattern("i");

    // Act and Assert
    assertEquals("Stralice.liddell@example.orgng", containsRegex.filter("String"));
  }

  /**
   * Test ContainsRegex new {@link ContainsRegex} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ContainsRegex}
   */
  @Test
  public void testContainsRegexNewContainsRegex() {
    // Arrange and Act
    ContainsRegex actualContainsRegex = new ContainsRegex();

    // Assert
    Location location = actualContainsRegex.getLocation();
    assertNull(location.getFileName());
    assertNull(actualContainsRegex.getDescription());
    assertNull(actualContainsRegex.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test ContainsString {@link ContainsString#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsString} (default constructor) Contains is {@code foo}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsString#filter(String)}
   */
  @Test
  public void testContainsStringFilter_givenContainsStringContainsIsFoo_thenReturnNull() {
    // Arrange
    ContainsString containsString = new ContainsString();
    containsString.setContains("foo");

    // Act and Assert
    assertNull(containsString.filter("String"));
  }

  /**
   * Test ContainsString {@link ContainsString#filter(String)}.
   * <ul>
   *   <li>Given {@link ContainsString} (default constructor).</li>
   *   <li>When {@code String}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsString#filter(String)}
   */
  @Test
  public void testContainsStringFilter_givenContainsString_whenString_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ContainsString()).filter("String"));
  }

  /**
   * Test ContainsString {@link ContainsString#filter(String)}.
   * <ul>
   *   <li>Then return {@code String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsString#filter(String)}
   */
  @Test
  public void testContainsStringFilter_thenReturnString() {
    // Arrange
    ContainsString containsString = new ContainsString();
    containsString.setContains("");

    // Act and Assert
    assertEquals("String", containsString.filter("String"));
  }

  /**
   * Test ContainsString new {@link ContainsString} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ContainsString}
   */
  @Test
  public void testContainsStringNewContainsString() {
    // Arrange and Act
    ContainsString actualContainsString = new ContainsString();

    // Assert
    Location location = actualContainsString.getLocation();
    assertNull(location.getFileName());
    assertNull(actualContainsString.getDescription());
    assertNull(actualContainsString.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test DeleteCharacters {@link DeleteCharacters#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return ready.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeleteCharacters#chain(Reader)}
   */
  @Test
  public void testDeleteCharactersChain_whenStringReaderWithFoo_thenReturnReady() throws IOException {
    // Arrange
    DeleteCharacters deleteCharacters = new DeleteCharacters();

    // Act and Assert
    assertTrue(deleteCharacters.chain(new StringReader("foo")).ready());
  }

  /**
   * Test DeleteCharacters {@link DeleteCharacters#filter(String)}.
   * <ul>
   *   <li>Given {@link DeleteCharacters} (default constructor).</li>
   *   <li>Then return {@code String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeleteCharacters#filter(String)}
   */
  @Test
  public void testDeleteCharactersFilter_givenDeleteCharacters_thenReturnString() {
    // Arrange, Act and Assert
    assertEquals("String", (new DeleteCharacters()).filter("String"));
  }

  /**
   * Test DeleteCharacters {@link DeleteCharacters#filter(String)}.
   * <ul>
   *   <li>Then return {@code Sing}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeleteCharacters#filter(String)}
   */
  @Test
  public void testDeleteCharactersFilter_thenReturnSing() {
    // Arrange
    DeleteCharacters deleteCharacters = new DeleteCharacters();
    deleteCharacters.setChars("Delete Chars");

    // Act and Assert
    assertEquals("Sing", deleteCharacters.filter("String"));
  }

  /**
   * Test DeleteCharacters new {@link DeleteCharacters} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DeleteCharacters}
   */
  @Test
  public void testDeleteCharactersNewDeleteCharacters() {
    // Arrange and Act
    DeleteCharacters actualDeleteCharacters = new DeleteCharacters();

    // Assert
    Location location = actualDeleteCharacters.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDeleteCharacters.getDescription());
    assertNull(actualDeleteCharacters.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test FileTokenizer new {@link FileTokenizer} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FileTokenizer}
   */
  @Test
  public void testFileTokenizerNewFileTokenizer() {
    // Arrange and Act
    FileTokenizer actualFileTokenizer = new FileTokenizer();

    // Assert
    assertEquals("", actualFileTokenizer.getPostToken());
    Location location = actualFileTokenizer.getLocation();
    assertNull(location.getFileName());
    assertNull(actualFileTokenizer.getDescription());
    assertNull(actualFileTokenizer.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test IgnoreBlank {@link IgnoreBlank#filter(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IgnoreBlank#filter(String)}
   */
  @Test
  public void testIgnoreBlankFilter_whenEmptyString_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new IgnoreBlank()).filter(""));
  }

  /**
   * Test IgnoreBlank {@link IgnoreBlank#filter(String)}.
   * <ul>
   *   <li>When {@code Line}.</li>
   *   <li>Then return {@code Line}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IgnoreBlank#filter(String)}
   */
  @Test
  public void testIgnoreBlankFilter_whenLine_thenReturnLine() {
    // Arrange, Act and Assert
    assertEquals("Line", (new IgnoreBlank()).filter("Line"));
  }

  /**
   * Test IgnoreBlank new {@link IgnoreBlank} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IgnoreBlank}
   */
  @Test
  public void testIgnoreBlankNewIgnoreBlank() {
    // Arrange and Act
    IgnoreBlank actualIgnoreBlank = new IgnoreBlank();

    // Assert
    Location location = actualIgnoreBlank.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIgnoreBlank.getDescription());
    assertNull(actualIgnoreBlank.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link TokenFilter#TokenFilter(Reader)}.
   * <p>
   * Method under test: {@link TokenFilter#TokenFilter(Reader)}
   */
  @Test
  public void testNewTokenFilter() {
    // Arrange and Act
    TokenFilter actualTokenFilter = new TokenFilter(new StringReader("foo"));

    // Assert
    assertNull(actualTokenFilter.getProject());
    assertFalse(actualTokenFilter.getInitialized());
  }

  /**
   * Test {@link TokenFilter#TokenFilter()}.
   * <p>
   * Method under test: {@link TokenFilter#TokenFilter()}
   */
  @Test
  public void testNewTokenFilter2() {
    // Arrange and Act
    TokenFilter actualTokenFilter = new TokenFilter();

    // Assert
    assertNull(actualTokenFilter.getProject());
    assertFalse(actualTokenFilter.getInitialized());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Flags is {@code janeUdoe@exampleUorg}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenReplaceRegexFlagsIsJaneUdoeExampleUorg_thenReturnMinusOne() throws IOException {
    // Arrange
    ReplaceRegex filter = new ReplaceRegex();
    filter.setFlags("janeUdoe@exampleUorg");
    filter.setPattern("jane.doe@example.org");

    TokenFilter tokenFilter = new TokenFilter(new StringReader("janeUdoe@exampleUorg"));
    tokenFilter.addReplaceRegex(filter);

    // Act and Assert
    assertEquals(-1, tokenFilter.read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Pattern is {@code jane.doe@example.org}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenReplaceRegexPatternIsJaneDoeExampleOrg_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    ReplaceRegex filter = new ReplaceRegex();
    filter.setPattern("jane.doe@example.org");

    TokenFilter tokenFilter = new TokenFilter(new StringReader("foo"));
    tokenFilter.addReplaceRegex(filter);

    // Act and Assert
    assertEquals(102, tokenFilter.read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new TokenFilter(new StringReader(""))).read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne2() throws IOException {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter(new StringReader(""));
    tokenFilter.addFileTokenizer(new FileTokenizer());

    // Act and Assert
    assertEquals(-1, tokenFilter.read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code i}.</li>
   *   <li>Then return one hundred five.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithI_thenReturnOneHundredFive() throws IOException {
    // Arrange
    ReplaceRegex filter = new ReplaceRegex();
    filter.setPattern("jane.doe@example.org");

    TokenFilter tokenFilter = new TokenFilter(new StringReader("i"));
    tokenFilter.addReplaceRegex(filter);

    // Act and Assert
    assertEquals(105, tokenFilter.read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code janeUdoe@exampleUorg}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithJaneUdoeExampleUorg_thenReturnMinusOne() throws IOException {
    // Arrange
    ReplaceRegex filter = new ReplaceRegex();
    filter.setPattern("jane.doe@example.org");

    TokenFilter tokenFilter = new TokenFilter(new StringReader("janeUdoe@exampleUorg"));
    tokenFilter.addReplaceRegex(filter);

    // Act and Assert
    assertEquals(-1, tokenFilter.read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter(Reader)} with in is {@link StringReader#StringReader(String)} addFileTokenizer {@link FileTokenizer} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenTokenFilterWithInIsStringReaderAddFileTokenizerFileTokenizer() throws IOException {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter(new StringReader("foo"));
    tokenFilter.addFileTokenizer(new FileTokenizer());

    // Act and Assert
    assertEquals(102, tokenFilter.read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter(Reader)} with in is {@link StringReader#StringReader(String)} addStringTokenizer {@link StringTokenizer} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenTokenFilterWithInIsStringReaderAddStringTokenizerStringTokenizer() throws IOException {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter(new StringReader("foo"));
    tokenFilter.addStringTokenizer(new StringTokenizer());

    // Act and Assert
    assertEquals(102, tokenFilter.read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter(Reader)} with in is {@link StringReader#StringReader(String)} addTrim {@link Trim} (default constructor).</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenTokenFilterWithInIsStringReaderAddTrimTrim_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter(new StringReader("foo"));
    tokenFilter.addTrim(new Trim());

    // Act and Assert
    assertEquals(102, tokenFilter.read());
  }

  /**
   * Test {@link TokenFilter#read()}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#read()}
   */
  @Test
  public void testRead_givenTokenFilterWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange, Act and Assert
    assertEquals(102, (new TokenFilter(new StringReader("foo"))).read());
  }

  /**
   * Test {@link TokenFilter#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link TokenFilter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnTokenFilter() throws IOException {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter();

    // Act
    Reader actualChainResult = tokenFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof TokenFilter);
    assertEquals("foo", ((TokenFilter) actualChainResult).readFully());
    assertNull(((TokenFilter) actualChainResult).getProject());
    assertFalse(((TokenFilter) actualChainResult).getInitialized());
    assertTrue(actualChainResult.ready());
  }

  /**
   * Test {@link TokenFilter#addLineTokenizer(LineTokenizer)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#addLineTokenizer(LineTokenizer)}
   */
  @Test
  public void testAddLineTokenizer_thenThrowBuildException() {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter();
    tokenFilter.addLineTokenizer(new LineTokenizer());

    // Act and Assert
    assertThrows(BuildException.class, () -> tokenFilter.addLineTokenizer(new LineTokenizer()));
  }

  /**
   * Test {@link TokenFilter#addStringTokenizer(StringTokenizer)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#addStringTokenizer(StringTokenizer)}
   */
  @Test
  public void testAddStringTokenizer_thenThrowBuildException() {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter();
    tokenFilter.addLineTokenizer(new LineTokenizer());

    // Act and Assert
    assertThrows(BuildException.class, () -> tokenFilter.addStringTokenizer(new StringTokenizer()));
  }

  /**
   * Test {@link TokenFilter#addFileTokenizer(FileTokenizer)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#addFileTokenizer(FileTokenizer)}
   */
  @Test
  public void testAddFileTokenizer_thenThrowBuildException() {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter();
    tokenFilter.addLineTokenizer(new LineTokenizer());

    // Act and Assert
    assertThrows(BuildException.class, () -> tokenFilter.addFileTokenizer(new FileTokenizer()));
  }

  /**
   * Test {@link TokenFilter#add(Tokenizer)} with {@code tokenizer}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#add(Tokenizer)}
   */
  @Test
  public void testAddWithTokenizer_thenThrowBuildException() {
    // Arrange
    TokenFilter tokenFilter = new TokenFilter();
    tokenFilter.addLineTokenizer(new LineTokenizer());

    // Act and Assert
    assertThrows(BuildException.class, () -> tokenFilter.add(new FileTokenizer()));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Flags is {@code g}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegexFlagsIsG_thenThrowBuildException() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setFlags("g");

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Flags is {@code i}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegexFlagsIsI_thenThrowBuildException() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setFlags("i");

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Flags is {@code i}.</li>
   *   <li>When {@code Line}.</li>
   *   <li>Then return {@code Lne}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegexFlagsIsI_whenLine_thenReturnLne() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setFlags("i");
    replaceRegex.setPattern("i");

    // Act and Assert
    assertEquals("Lne", replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Flags is {@code m}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegexFlagsIsM_thenThrowBuildException() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setFlags("m");

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Flags is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegexFlagsIsNull_thenThrowBuildException() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setFlags(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Flags is {@code s}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegexFlagsIsS_thenThrowBuildException() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setFlags("s");

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Pattern is {@code i}.</li>
   *   <li>When {@code Line}.</li>
   *   <li>Then return {@code Lne}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegexPatternIsI_whenLine_thenReturnLne() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setPattern("i");

    // Act and Assert
    assertEquals("Lne", replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@code Line}.</li>
   *   <li>Then return {@code Lne}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegexProjectIsProject_whenLine_thenReturnLne() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setProject(new Project());
    replaceRegex.setPattern("i");

    // Act and Assert
    assertEquals("Lne", replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegex} (default constructor).</li>
   *   <li>When {@code Line}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_givenReplaceRegex_whenLine_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ReplaceRegex()).filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Then return {@code Lalice.liddell@example.orgne}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_thenReturnLaliceLiddellExampleOrgne() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setReplace("alice.liddell@example.org");
    replaceRegex.setPattern("i");

    // Act and Assert
    assertEquals("Lalice.liddell@example.orgne", replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex {@link ReplaceRegex#filter(String)}.
   * <ul>
   *   <li>Then return {@code Line}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegex#filter(String)}
   */
  @Test
  public void testReplaceRegexFilter_thenReturnLine() {
    // Arrange
    ReplaceRegex replaceRegex = new ReplaceRegex();
    replaceRegex.setPattern("jane.doe@example.org");

    // Act and Assert
    assertEquals("Line", replaceRegex.filter("Line"));
  }

  /**
   * Test ReplaceRegex new {@link ReplaceRegex} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ReplaceRegex}
   */
  @Test
  public void testReplaceRegexNewReplaceRegex() {
    // Arrange and Act
    ReplaceRegex actualReplaceRegex = new ReplaceRegex();

    // Assert
    Location location = actualReplaceRegex.getLocation();
    assertNull(location.getFileName());
    assertNull(actualReplaceRegex.getDescription());
    assertNull(actualReplaceRegex.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test ReplaceString {@link ReplaceString#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceString} (default constructor) From is {@code jane.doe@example.org}.</li>
   *   <li>Then return {@code Line}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceString#filter(String)}
   */
  @Test
  public void testReplaceStringFilter_givenReplaceStringFromIsJaneDoeExampleOrg_thenReturnLine() {
    // Arrange
    ReplaceString replaceString = new ReplaceString();
    replaceString.setFrom("jane.doe@example.org");

    // Act and Assert
    assertEquals("Line", replaceString.filter("Line"));
  }

  /**
   * Test ReplaceString {@link ReplaceString#filter(String)}.
   * <ul>
   *   <li>Given {@link ReplaceString} (default constructor).</li>
   *   <li>When {@code Line}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceString#filter(String)}
   */
  @Test
  public void testReplaceStringFilter_givenReplaceString_whenLine_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ReplaceString()).filter("Line"));
  }

  /**
   * Test ReplaceString {@link ReplaceString#filter(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceString#filter(String)}
   */
  @Test
  public void testReplaceStringFilter_whenEmptyString_thenReturnEmptyString() {
    // Arrange
    ReplaceString replaceString = new ReplaceString();
    replaceString.setFrom("jane.doe@example.org");

    // Act and Assert
    assertEquals("", replaceString.filter(""));
  }

  /**
   * Test ReplaceString new {@link ReplaceString} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ReplaceString}
   */
  @Test
  public void testReplaceStringNewReplaceString() {
    // Arrange and Act
    ReplaceString actualReplaceString = new ReplaceString();

    // Assert
    Location location = actualReplaceString.getLocation();
    assertNull(location.getFileName());
    assertNull(actualReplaceString.getDescription());
    assertNull(actualReplaceString.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link TokenFilter#resolveBackSlash(String)}.
   * <p>
   * Method under test: {@link TokenFilter#resolveBackSlash(String)}
   */
  @Test
  public void testResolveBackSlash() {
    // Arrange, Act and Assert
    assertEquals("Input", TokenFilter.resolveBackSlash("Input"));
  }

  /**
   * Test {@link TokenFilter#convertRegexOptions(String)}.
   * <ul>
   *   <li>When {@code Flags}.</li>
   *   <li>Then return {@code 65552}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#convertRegexOptions(String)}
   */
  @Test
  public void testConvertRegexOptions_whenFlags_thenReturn65552() {
    // Arrange, Act and Assert
    assertEquals(65552, TokenFilter.convertRegexOptions("Flags"));
  }

  /**
   * Test {@link TokenFilter#convertRegexOptions(String)}.
   * <ul>
   *   <li>When {@code i}.</li>
   *   <li>Then return two hundred fifty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#convertRegexOptions(String)}
   */
  @Test
  public void testConvertRegexOptions_whenI_thenReturnTwoHundredFiftySix() {
    // Arrange, Act and Assert
    assertEquals(256, TokenFilter.convertRegexOptions("i"));
  }

  /**
   * Test {@link TokenFilter#convertRegexOptions(String)}.
   * <ul>
   *   <li>When {@code m}.</li>
   *   <li>Then return {@code 4096}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#convertRegexOptions(String)}
   */
  @Test
  public void testConvertRegexOptions_whenM_thenReturn4096() {
    // Arrange, Act and Assert
    assertEquals(4096, TokenFilter.convertRegexOptions("m"));
  }

  /**
   * Test {@link TokenFilter#convertRegexOptions(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenFilter#convertRegexOptions(String)}
   */
  @Test
  public void testConvertRegexOptions_whenNull_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, TokenFilter.convertRegexOptions(null));
  }

  /**
   * Test StringTokenizer new {@link StringTokenizer} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link StringTokenizer}
   */
  @Test
  public void testStringTokenizerNewStringTokenizer() {
    // Arrange and Act
    StringTokenizer actualStringTokenizer = new StringTokenizer();

    // Assert
    assertEquals("", actualStringTokenizer.getPostToken());
    Location location = actualStringTokenizer.getLocation();
    assertNull(location.getFileName());
    assertNull(actualStringTokenizer.getDescription());
    assertNull(actualStringTokenizer.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test Trim {@link Trim#filter(String)}.
   * <p>
   * Method under test: {@link Trim#filter(String)}
   */
  @Test
  public void testTrimFilter() {
    // Arrange, Act and Assert
    assertEquals("Line", (new Trim()).filter("Line"));
  }

  /**
   * Test Trim new {@link Trim} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Trim}
   */
  @Test
  public void testTrimNewTrim() {
    // Arrange and Act
    Trim actualTrim = new Trim();

    // Assert
    Location location = actualTrim.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTrim.getDescription());
    assertNull(actualTrim.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
