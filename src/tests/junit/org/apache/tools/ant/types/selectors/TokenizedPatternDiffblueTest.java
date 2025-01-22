package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class TokenizedPatternDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TokenizedPattern#TokenizedPattern(String, String[])}
   *   <li>{@link TokenizedPattern#getPattern()}
   *   <li>{@link TokenizedPattern#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    TokenizedPattern actualTokenizedPattern = new TokenizedPattern("Pattern", new String[]{"ABC123"});
    String actualPattern = actualTokenizedPattern.getPattern();

    // Assert
    assertEquals("Pattern", actualPattern);
    assertEquals("Pattern", actualTokenizedPattern.toString());
  }

  /**
   * Test {@link TokenizedPattern#TokenizedPattern(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Pattern is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#TokenizedPattern(String)}
   */
  @Test
  public void testNewTokenizedPattern_whenEmptyString_thenReturnPatternIsEmptyString() {
    // Arrange and Act
    TokenizedPattern actualTokenizedPattern = new TokenizedPattern("");

    // Assert
    assertEquals("", actualTokenizedPattern.getPattern());
    assertEquals("", actualTokenizedPattern.toString());
    assertEquals(0, actualTokenizedPattern.depth());
  }

  /**
   * Test {@link TokenizedPattern#TokenizedPattern(String)}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#TokenizedPattern(String)}
   */
  @Test
  public void testNewTokenizedPattern_whenPattern_thenReturnPattern() {
    // Arrange and Act
    TokenizedPattern actualTokenizedPattern = new TokenizedPattern("Pattern");

    // Assert
    assertEquals("Pattern", actualTokenizedPattern.getPattern());
    assertEquals("Pattern", actualTokenizedPattern.toString());
    assertEquals(1, actualTokenizedPattern.depth());
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**org.apache.tools.ant.types.selectors.TokenizedPattern");

    // Act and Assert
    assertFalse(tokenizedPattern.matchPath(new TokenizedPath("Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#EMPTY_PATTERN}.</li>
   *   <li>When {@link TokenizedPath#EMPTY_PATH}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenEmpty_pattern_whenEmpty_path_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TokenizedPattern.EMPTY_PATTERN.matchPath(TokenizedPath.EMPTY_PATH, true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#EMPTY_PATTERN}.</li>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenEmpty_pattern_whenTokenizedPathWithPath_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TokenizedPattern.EMPTY_PATTERN.matchPath(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern");

    // Act and Assert
    assertTrue(tokenizedPattern.matchPath(new TokenizedPath("Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern2() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern");

    // Act and Assert
    assertTrue(tokenizedPattern.matchPath(new TokenizedPath("Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern**}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern3() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern**");

    // Act and Assert
    assertTrue(tokenizedPattern.matchPath(new TokenizedPath("Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern**}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern4() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern**");

    // Act and Assert
    assertTrue(tokenizedPattern.matchPath(new TokenizedPath("**Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern**}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern5() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern**");

    // Act and Assert
    assertFalse(tokenizedPattern.matchPath(new TokenizedPath(SelectorUtils.DEEP_TREE_MATCH), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPatternIs42() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("42");

    // Act and Assert
    assertFalse(tokenizedPattern.matchPath(new TokenizedPath(SelectorUtils.DEEP_TREE_MATCH), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@code ****}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPatternIsAsteriskAsteriskAsteriskAsterisk() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("****");

    // Act and Assert
    assertTrue(tokenizedPattern.matchPath(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPatternIsDeep_tree_match() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern(SelectorUtils.DEEP_TREE_MATCH);

    // Act and Assert
    assertTrue(tokenizedPattern.matchPath(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   *   <li>When {@link TokenizedPath#EMPTY_PATH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPatternIsDeep_tree_match_whenEmpty_path() {
    // Arrange, Act and Assert
    assertTrue((new TokenizedPattern(SelectorUtils.DEEP_TREE_MATCH)).matchPath(TokenizedPath.EMPTY_PATH, true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   *   <li>When {@link TokenizedPath#EMPTY_PATH}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern_whenEmpty_path_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TokenizedPattern("Pattern")).matchPath(TokenizedPath.EMPTY_PATH, true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern}.</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern_whenFalse_thenReturnFalse() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern");

    // Act and Assert
    assertFalse(tokenizedPattern.matchPath(new TokenizedPath("Path"), false));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern_whenFalse_thenReturnTrue() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern");

    // Act and Assert
    assertTrue(tokenizedPattern.matchPath(new TokenizedPath("Pattern"), false));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern_whenTokenizedPathWithPath() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern");

    // Act and Assert
    assertFalse(tokenizedPattern.matchPath(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern}.</li>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern_whenTokenizedPathWithPath2() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern");

    // Act and Assert
    assertFalse(tokenizedPattern.matchPath(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern**}.</li>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_givenTokenizedPatternWithPattern_whenTokenizedPathWithPath3() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern**");

    // Act and Assert
    assertFalse(tokenizedPattern.matchPath(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}.
   * <ul>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with path is {@code **Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchPath(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchPath_whenTokenizedPathWithPathIsPattern() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern");

    // Act and Assert
    assertTrue(tokenizedPattern.matchPath(new TokenizedPath("**Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("****");

    // Act and Assert
    assertTrue(tokenizedPattern.matchStartOf(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf2() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**org.apache.tools.ant.types.selectors.TokenizedPattern");

    // Act and Assert
    assertFalse(tokenizedPattern.matchStartOf(new TokenizedPath("Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#EMPTY_PATTERN}.</li>
   *   <li>When {@link TokenizedPath#EMPTY_PATH}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenEmpty_pattern_whenEmpty_path_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(TokenizedPattern.EMPTY_PATTERN.matchStartOf(TokenizedPath.EMPTY_PATH, true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#EMPTY_PATTERN}.</li>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenEmpty_pattern_whenTokenizedPathWithPath_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TokenizedPattern.EMPTY_PATTERN.matchStartOf(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern");

    // Act and Assert
    assertTrue(tokenizedPattern.matchStartOf(new TokenizedPath("Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern2() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern");

    // Act and Assert
    assertTrue(tokenizedPattern.matchStartOf(new TokenizedPath("Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern**}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern3() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern**");

    // Act and Assert
    assertTrue(tokenizedPattern.matchStartOf(new TokenizedPath("Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern**}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern4() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern**");

    // Act and Assert
    assertTrue(tokenizedPattern.matchStartOf(new TokenizedPath("**Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern**}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern5() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern**");

    // Act and Assert
    assertFalse(tokenizedPattern.matchStartOf(new TokenizedPath(SelectorUtils.DEEP_TREE_MATCH), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPatternIs42() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("42");

    // Act and Assert
    assertFalse(tokenizedPattern.matchStartOf(new TokenizedPath(SelectorUtils.DEEP_TREE_MATCH), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPatternIsDeep_tree_match() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern(SelectorUtils.DEEP_TREE_MATCH);

    // Act and Assert
    assertTrue(tokenizedPattern.matchStartOf(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   *   <li>When {@link TokenizedPath#EMPTY_PATH}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern_whenEmpty_path_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new TokenizedPattern("Pattern")).matchStartOf(TokenizedPath.EMPTY_PATH, true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern}.</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern_whenFalse_thenReturnFalse() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern");

    // Act and Assert
    assertFalse(tokenizedPattern.matchStartOf(new TokenizedPath("Path"), false));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern_whenFalse_thenReturnTrue() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern");

    // Act and Assert
    assertTrue(tokenizedPattern.matchStartOf(new TokenizedPath("Pattern"), false));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern_whenTokenizedPathWithPath() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern");

    // Act and Assert
    assertFalse(tokenizedPattern.matchStartOf(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code **Pattern}.</li>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern_whenTokenizedPathWithPath2() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern");

    // Act and Assert
    assertFalse(tokenizedPattern.matchStartOf(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern**}.</li>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_givenTokenizedPatternWithPattern_whenTokenizedPathWithPath3() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern**");

    // Act and Assert
    assertFalse(tokenizedPattern.matchStartOf(new TokenizedPath("Path"), true));
  }

  /**
   * Test {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}.
   * <ul>
   *   <li>When {@link TokenizedPath#TokenizedPath(String)} with path is {@code **Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#matchStartOf(TokenizedPath, boolean)}
   */
  @Test
  public void testMatchStartOf_whenTokenizedPathWithPathIsPattern() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("**Pattern");

    // Act and Assert
    assertTrue(tokenizedPattern.matchStartOf(new TokenizedPath("**Pattern"), true));
  }

  /**
   * Test {@link TokenizedPattern#equals(Object)}, and {@link TokenizedPattern#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TokenizedPattern#equals(Object)}
   *   <li>{@link TokenizedPattern#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    TokenizedPattern tokenizedPattern = TokenizedPattern.EMPTY_PATTERN;
    TokenizedPattern tokenizedPattern2 = TokenizedPattern.EMPTY_PATTERN;

    // Act and Assert
    assertEquals(tokenizedPattern, tokenizedPattern2);
    int expectedHashCodeResult = tokenizedPattern.hashCode();
    assertEquals(expectedHashCodeResult, tokenizedPattern2.hashCode());
  }

  /**
   * Test {@link TokenizedPattern#equals(Object)}, and {@link TokenizedPattern#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TokenizedPattern#equals(Object)}
   *   <li>{@link TokenizedPattern#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    TokenizedPattern tokenizedPattern = TokenizedPattern.EMPTY_PATTERN;

    // Act and Assert
    assertEquals(tokenizedPattern, tokenizedPattern);
    int expectedHashCodeResult = tokenizedPattern.hashCode();
    assertEquals(expectedHashCodeResult, tokenizedPattern.hashCode());
  }

  /**
   * Test {@link TokenizedPattern#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new TokenizedPattern("Pattern"), TokenizedPattern.EMPTY_PATTERN);
  }

  /**
   * Test {@link TokenizedPattern#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(TokenizedPattern.EMPTY_PATTERN, null);
  }

  /**
   * Test {@link TokenizedPattern#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(TokenizedPattern.EMPTY_PATTERN, "Different type to TokenizedPattern");
  }

  /**
   * Test {@link TokenizedPattern#depth()}.
   * <p>
   * Method under test: {@link TokenizedPattern#depth()}
   */
  @Test
  public void testDepth() {
    // Arrange, Act and Assert
    assertEquals(0, TokenizedPattern.EMPTY_PATTERN.depth());
  }

  /**
   * Test {@link TokenizedPattern#containsPattern(String)}.
   * <p>
   * Method under test: {@link TokenizedPattern#containsPattern(String)}
   */
  @Test
  public void testContainsPattern() {
    // Arrange, Act and Assert
    assertFalse(TokenizedPattern.EMPTY_PATTERN.containsPattern("Pat"));
  }

  /**
   * Test {@link TokenizedPattern#rtrimWildcardTokens()}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#EMPTY_PATTERN}.</li>
   *   <li>Then return {@link TokenizedPath#EMPTY_PATH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#rtrimWildcardTokens()}
   */
  @Test
  public void testRtrimWildcardTokens_givenEmpty_pattern_thenReturnEmpty_path() {
    // Arrange and Act
    TokenizedPath actualRtrimWildcardTokensResult = TokenizedPattern.EMPTY_PATTERN.rtrimWildcardTokens();

    // Assert
    assertSame(actualRtrimWildcardTokensResult.EMPTY_PATH, actualRtrimWildcardTokensResult);
  }

  /**
   * Test {@link TokenizedPattern#rtrimWildcardTokens()}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#rtrimWildcardTokens()}
   */
  @Test
  public void testRtrimWildcardTokens_givenTokenizedPatternWithPatternIsAsterisk() {
    // Arrange and Act
    TokenizedPath actualRtrimWildcardTokensResult = (new TokenizedPattern("*")).rtrimWildcardTokens();

    // Assert
    assertSame(actualRtrimWildcardTokensResult.EMPTY_PATH, actualRtrimWildcardTokensResult);
  }

  /**
   * Test {@link TokenizedPattern#rtrimWildcardTokens()}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@code ?}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#rtrimWildcardTokens()}
   */
  @Test
  public void testRtrimWildcardTokens_givenTokenizedPatternWithPatternIsQuestionMark() {
    // Arrange and Act
    TokenizedPath actualRtrimWildcardTokensResult = (new TokenizedPattern("?")).rtrimWildcardTokens();

    // Assert
    assertSame(actualRtrimWildcardTokensResult.EMPTY_PATH, actualRtrimWildcardTokensResult);
  }

  /**
   * Test {@link TokenizedPattern#rtrimWildcardTokens()}.
   * <ul>
   *   <li>Then return toString is {@code ABC123/Tokens}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#rtrimWildcardTokens()}
   */
  @Test
  public void testRtrimWildcardTokens_thenReturnToStringIsAbc123Tokens() {
    // Arrange and Act
    TokenizedPath actualRtrimWildcardTokensResult = (new TokenizedPattern("Pattern", new String[]{"ABC123", "Tokens"}))
        .rtrimWildcardTokens();

    // Assert
    assertEquals("ABC123/Tokens", actualRtrimWildcardTokensResult.toString());
    TokenizedPattern toPatternResult = actualRtrimWildcardTokensResult.toPattern();
    assertEquals("ABC123/Tokens", toPatternResult.getPattern());
    assertEquals("ABC123/Tokens", toPatternResult.toString());
    assertEquals(2, actualRtrimWildcardTokensResult.depth());
    assertEquals(2, toPatternResult.depth());
    assertArrayEquals(new String[]{"ABC123", "Tokens"}, actualRtrimWildcardTokensResult.getTokens());
  }

  /**
   * Test {@link TokenizedPattern#rtrimWildcardTokens()}.
   * <ul>
   *   <li>Then return toString is {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#rtrimWildcardTokens()}
   */
  @Test
  public void testRtrimWildcardTokens_thenReturnToStringIsPattern() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("Pattern");

    // Act
    TokenizedPath actualRtrimWildcardTokensResult = tokenizedPattern.rtrimWildcardTokens();

    // Assert
    assertEquals("Pattern", actualRtrimWildcardTokensResult.toString());
    assertEquals(1, actualRtrimWildcardTokensResult.depth());
    assertEquals(tokenizedPattern, actualRtrimWildcardTokensResult.toPattern());
    assertArrayEquals(new String[]{"Pattern"}, actualRtrimWildcardTokensResult.getTokens());
  }

  /**
   * Test {@link TokenizedPattern#endsWith(String)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#EMPTY_PATTERN}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#endsWith(String)}
   */
  @Test
  public void testEndsWith_givenEmpty_pattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(TokenizedPattern.EMPTY_PATTERN.endsWith("foo"));
  }

  /**
   * Test {@link TokenizedPattern#endsWith(String)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with pattern is {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#endsWith(String)}
   */
  @Test
  public void testEndsWith_givenTokenizedPatternWithPatternIsFoo_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new TokenizedPattern("foo")).endsWith("foo"));
  }

  /**
   * Test {@link TokenizedPattern#endsWith(String)}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#TokenizedPattern(String)} with {@code Pattern}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#endsWith(String)}
   */
  @Test
  public void testEndsWith_givenTokenizedPatternWithPattern_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TokenizedPattern("Pattern")).endsWith("foo"));
  }

  /**
   * Test {@link TokenizedPattern#withoutLastToken()}.
   * <p>
   * Method under test: {@link TokenizedPattern#withoutLastToken()}
   */
  @Test
  public void testWithoutLastToken() {
    // Arrange
    TokenizedPattern tokenizedPattern = new TokenizedPattern("can't strip a token from nothing",
        new String[]{"ABC123", ""});

    // Act and Assert
    assertEquals(tokenizedPattern, tokenizedPattern.withoutLastToken());
  }

  /**
   * Test {@link TokenizedPattern#withoutLastToken()}.
   * <ul>
   *   <li>Given {@link TokenizedPattern#EMPTY_PATTERN}.</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#withoutLastToken()}
   */
  @Test
  public void testWithoutLastToken_givenEmpty_pattern_thenThrowIllegalStateException() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> TokenizedPattern.EMPTY_PATTERN.withoutLastToken());
  }

  /**
   * Test {@link TokenizedPattern#withoutLastToken()}.
   * <ul>
   *   <li>Then return {@link TokenizedPattern#EMPTY_PATTERN}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TokenizedPattern#withoutLastToken()}
   */
  @Test
  public void testWithoutLastToken_thenReturnEmpty_pattern() {
    // Arrange and Act
    TokenizedPattern actualWithoutLastTokenResult = (new TokenizedPattern("can't strip a token from nothing"))
        .withoutLastToken();

    // Assert
    assertSame(actualWithoutLastTokenResult.EMPTY_PATTERN, actualWithoutLastTokenResult);
  }
}
