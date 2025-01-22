package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.filters.ReplaceTokens.Token;
import org.apache.tools.ant.types.Parameter;
import org.junit.Test;

public class ReplaceTokensDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ReplaceTokens#ReplaceTokens(Reader)}
   *   <li>{@link ReplaceTokens#setBeginToken(String)}
   *   <li>{@link ReplaceTokens#setEndToken(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ReplaceTokens actualReplaceTokens = new ReplaceTokens(new StringReader("foo"));
    actualReplaceTokens.setBeginToken("ABC123");
    actualReplaceTokens.setEndToken("ABC123");

    // Assert
    assertNull(actualReplaceTokens.getParameters());
    assertNull(actualReplaceTokens.getProject());
    assertFalse(actualReplaceTokens.getInitialized());
  }

  /**
   * Test {@link ReplaceTokens#ReplaceTokens()}.
   * <p>
   * Method under test: {@link ReplaceTokens#ReplaceTokens()}
   */
  @Test
  public void testNewReplaceTokens() {
    // Arrange and Act
    ReplaceTokens actualReplaceTokens = new ReplaceTokens();

    // Assert
    assertNull(actualReplaceTokens.getParameters());
    assertNull(actualReplaceTokens.getProject());
    assertFalse(actualReplaceTokens.getInitialized());
  }

  /**
   * Test {@link ReplaceTokens#read()}.
   * <ul>
   *   <li>Given {@link ReplaceTokens#ReplaceTokens(Reader)} with in is {@link StringReader#StringReader(String)} skip three.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceTokens#read()}
   */
  @Test
  public void testRead_givenReplaceTokensWithInIsStringReaderSkipThree_thenReturnMinusOne()
      throws IOException, IllegalArgumentException {
    // Arrange
    ReplaceTokens replaceTokens = new ReplaceTokens(new StringReader("foo"));
    replaceTokens.skip(3L);

    // Act and Assert
    assertEquals(-1, replaceTokens.read());
    assertTrue(replaceTokens.getInitialized());
  }

  /**
   * Test {@link ReplaceTokens#read()}.
   * <ul>
   *   <li>Given {@link ReplaceTokens#ReplaceTokens(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceTokens#read()}
   */
  @Test
  public void testRead_givenReplaceTokensWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    ReplaceTokens replaceTokens = new ReplaceTokens(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, replaceTokens.read());
    assertTrue(replaceTokens.getInitialized());
  }

  /**
   * Test {@link ReplaceTokens#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code 42}.</li>
   *   <li>Then return fifty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceTokens#read()}
   */
  @Test
  public void testRead_givenStringReaderWith42_thenReturnFiftyTwo() throws IOException {
    // Arrange
    Token token = new Token();
    token.setKey("Key");
    token.setValue("42");

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("propertiesfile");
    parameter.setValue("42");

    ReplaceTokens replaceTokens = new ReplaceTokens(new StringReader("42"));
    replaceTokens.setParameters(parameter);
    replaceTokens.addConfiguredToken(token);

    // Act and Assert
    assertEquals(52, replaceTokens.read());
    assertTrue(replaceTokens.getInitialized());
  }

  /**
   * Test {@link ReplaceTokens#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceTokens#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    ReplaceTokens replaceTokens = new ReplaceTokens(new StringReader(""));

    // Act and Assert
    assertEquals(-1, replaceTokens.read());
    assertTrue(replaceTokens.getInitialized());
  }

  /**
   * Test {@link ReplaceTokens#read()}.
   * <ul>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceTokens#read()}
   */
  @Test
  public void testRead_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    Token token = new Token();
    token.setKey("Key");
    token.setValue("42");

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("propertiesfile");
    parameter.setValue("42");

    ReplaceTokens replaceTokens = new ReplaceTokens(new StringReader("foo"));
    replaceTokens.setParameters(parameter);
    replaceTokens.addConfiguredToken(token);

    // Act and Assert
    assertEquals(102, replaceTokens.read());
    assertTrue(replaceTokens.getInitialized());
  }

  /**
   * Test {@link ReplaceTokens#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link ReplaceTokens}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceTokens#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnReplaceTokens() throws IOException {
    // Arrange
    ReplaceTokens replaceTokens = new ReplaceTokens();

    // Act
    Reader actualChainResult = replaceTokens.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof ReplaceTokens);
    assertEquals("foo", ((ReplaceTokens) actualChainResult).readFully());
    assertNull(((ReplaceTokens) actualChainResult).getParameters());
    assertNull(((ReplaceTokens) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((ReplaceTokens) actualChainResult).getInitialized());
  }

  /**
   * Test Token getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Token}
   *   <li>{@link Token#setKey(String)}
   *   <li>{@link Token#setValue(String)}
   *   <li>{@link Token#getKey()}
   *   <li>{@link Token#getValue()}
   * </ul>
   */
  @Test
  public void testTokenGettersAndSetters() {
    // Arrange and Act
    Token actualToken = new Token();
    actualToken.setKey("Key");
    actualToken.setValue("42");
    String actualKey = actualToken.getKey();

    // Assert
    assertEquals("42", actualToken.getValue());
    assertEquals("Key", actualKey);
  }
}
