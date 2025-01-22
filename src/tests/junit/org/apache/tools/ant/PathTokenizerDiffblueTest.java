package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.NoSuchElementException;
import org.junit.Test;

public class PathTokenizerDiffblueTest {
  /**
   * Test {@link PathTokenizer#PathTokenizer(String)}.
   * <p>
   * Method under test: {@link PathTokenizer#PathTokenizer(String)}
   */
  @Test
  public void testNewPathTokenizer() {
    // Arrange, Act and Assert
    assertTrue((new PathTokenizer("Path")).hasMoreTokens());
  }

  /**
   * Test {@link PathTokenizer#hasMoreTokens()}.
   * <ul>
   *   <li>Given {@link PathTokenizer#PathTokenizer(String)} with path is empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathTokenizer#hasMoreTokens()}
   */
  @Test
  public void testHasMoreTokens_givenPathTokenizerWithPathIsEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PathTokenizer("")).hasMoreTokens());
  }

  /**
   * Test {@link PathTokenizer#hasMoreTokens()}.
   * <ul>
   *   <li>Given {@link PathTokenizer#PathTokenizer(String)} with {@code Path}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathTokenizer#hasMoreTokens()}
   */
  @Test
  public void testHasMoreTokens_givenPathTokenizerWithPath_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PathTokenizer("Path")).hasMoreTokens());
  }

  /**
   * Test {@link PathTokenizer#nextToken()}.
   * <ul>
   *   <li>Given {@link PathTokenizer#PathTokenizer(String)} with path is {@code .}.</li>
   *   <li>Then return {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathTokenizer#nextToken()}
   */
  @Test
  public void testNextToken_givenPathTokenizerWithPathIsDot_thenReturnDot() throws NoSuchElementException {
    // Arrange
    PathTokenizer pathTokenizer = new PathTokenizer(".");

    // Act and Assert
    assertEquals(".", pathTokenizer.nextToken());
    assertFalse(pathTokenizer.hasMoreTokens());
  }

  /**
   * Test {@link PathTokenizer#nextToken()}.
   * <ul>
   *   <li>Given {@link PathTokenizer#PathTokenizer(String)} with {@code Path}.</li>
   *   <li>Then return {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PathTokenizer#nextToken()}
   */
  @Test
  public void testNextToken_givenPathTokenizerWithPath_thenReturnPath() throws NoSuchElementException {
    // Arrange
    PathTokenizer pathTokenizer = new PathTokenizer("Path");

    // Act and Assert
    assertEquals("Path", pathTokenizer.nextToken());
    assertFalse(pathTokenizer.hasMoreTokens());
  }
}
