package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.RegularExpression;
import org.junit.Test;

public class MatchesDiffblueTest {
  /**
   * Test {@link Matches#setPattern(String)}.
   * <ul>
   *   <li>Given {@link Matches} (default constructor) Pattern is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#setPattern(String)}
   */
  @Test
  public void testSetPattern_givenMatchesPatternIsNull_thenThrowBuildException() {
    // Arrange
    Matches matches = new Matches();
    matches.setPattern(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> matches.setPattern("Pattern"));
  }

  /**
   * Test {@link Matches#addRegexp(RegularExpression)}.
   * <ul>
   *   <li>Given {@link Matches} (default constructor) Pattern is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#addRegexp(RegularExpression)}
   */
  @Test
  public void testAddRegexp_givenMatchesPatternIsNull_thenThrowBuildException() {
    // Arrange
    Matches matches = new Matches();
    matches.setPattern(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> matches.addRegexp(new RegularExpression()));
  }

  /**
   * Test {@link Matches#eval()}.
   * <ul>
   *   <li>Given {@link Matches} (default constructor) Casesensitive is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#eval()}
   */
  @Test
  public void testEval_givenMatchesCasesensitiveIsTrue_thenReturnTrue() throws BuildException {
    // Arrange
    Matches matches = new Matches();
    matches.setString("foo");
    matches.setPattern("foo");
    matches.setProject(null);
    matches.setCasesensitive(true);
    matches.setMultiline(false);
    matches.setSingleLine(false);

    // Act and Assert
    assertTrue(matches.eval());
  }

  /**
   * Test {@link Matches#eval()}.
   * <ul>
   *   <li>Given {@link Matches} (default constructor) Multiline is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#eval()}
   */
  @Test
  public void testEval_givenMatchesMultilineIsTrue_thenReturnTrue() throws BuildException {
    // Arrange
    Matches matches = new Matches();
    matches.setString("foo");
    matches.setPattern("foo");
    matches.setProject(null);
    matches.setCasesensitive(false);
    matches.setMultiline(true);
    matches.setSingleLine(false);

    // Act and Assert
    assertTrue(matches.eval());
  }

  /**
   * Test {@link Matches#eval()}.
   * <ul>
   *   <li>Given {@link Matches} (default constructor) Pattern is {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#eval()}
   */
  @Test
  public void testEval_givenMatchesPatternIsFoo_thenReturnTrue() throws BuildException {
    // Arrange
    Matches matches = new Matches();
    matches.setString("foo");
    matches.setPattern("foo");
    matches.setProject(null);
    matches.setCasesensitive(false);
    matches.setMultiline(false);
    matches.setSingleLine(false);

    // Act and Assert
    assertTrue(matches.eval());
  }

  /**
   * Test {@link Matches#eval()}.
   * <ul>
   *   <li>Given {@link Matches} (default constructor) SingleLine is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#eval()}
   */
  @Test
  public void testEval_givenMatchesSingleLineIsTrue_thenReturnTrue() throws BuildException {
    // Arrange
    Matches matches = new Matches();
    matches.setString("foo");
    matches.setPattern("foo");
    matches.setProject(null);
    matches.setCasesensitive(false);
    matches.setMultiline(false);
    matches.setSingleLine(true);

    // Act and Assert
    assertTrue(matches.eval());
  }

  /**
   * Test {@link Matches#eval()}.
   * <ul>
   *   <li>Given {@link Matches} (default constructor) String is {@code String}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#eval()}
   */
  @Test
  public void testEval_givenMatchesStringIsString_thenReturnFalse() throws BuildException {
    // Arrange
    Matches matches = new Matches();
    matches.setString("String");
    matches.setPattern("foo");
    matches.setProject(null);
    matches.setCasesensitive(false);
    matches.setMultiline(false);
    matches.setSingleLine(false);

    // Act and Assert
    assertFalse(matches.eval());
  }

  /**
   * Test {@link Matches#eval()}.
   * <ul>
   *   <li>Given {@link Matches} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#eval()}
   */
  @Test
  public void testEval_givenMatches_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Matches()).eval());
  }

  /**
   * Test {@link Matches#eval()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Matches#eval()}
   */
  @Test
  public void testEval_thenThrowBuildException() throws BuildException {
    // Arrange
    Matches matches = new Matches();
    matches.setString("Parameter string is required in matches.");

    // Act and Assert
    assertThrows(BuildException.class, () -> matches.eval());
  }

  /**
   * Test new {@link Matches} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Matches}
   */
  @Test
  public void testNewMatches() {
    // Arrange and Act
    Matches actualMatches = new Matches();

    // Assert
    Location location = actualMatches.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMatches.getDescription());
    assertNull(actualMatches.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
