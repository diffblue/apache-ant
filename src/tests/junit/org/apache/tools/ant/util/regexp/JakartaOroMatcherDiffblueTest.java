package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.apache.oro.text.regex.Pattern;
import org.apache.oro.text.regex.Perl5Matcher;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JakartaOroMatcherDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link JakartaOroMatcher}
  *   <li>{@link JakartaOroMatcher#setPattern(String)}
  *   <li>{@link JakartaOroMatcher#getPattern()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    JakartaOroMatcher actualJakartaOroMatcher = new JakartaOroMatcher();
    actualJakartaOroMatcher.setPattern("Pattern");

    // Assert
    assertEquals("Pattern", actualJakartaOroMatcher.getPattern());
    Perl5Matcher perl5Matcher = actualJakartaOroMatcher.matcher;
    assertFalse(perl5Matcher.isMultiline());
    assertNull(perl5Matcher.getMatch());
  }

  /**
   * Method under test: default or parameterless constructor of {@link JakartaOroMatcher}
   */
  @Test
  public void testConstructor2() {
    // Arrange, Act and Assert
    Perl5Matcher perl5Matcher = (new JakartaOroMatcher()).matcher;
    assertFalse(perl5Matcher.isMultiline());
    assertNull(perl5Matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act
    Pattern actualCompiledPattern = jakartaOroMatcher.getCompiledPattern(1);

    // Assert
    assertEquals(0, actualCompiledPattern.getOptions());
    assertEquals("Pattern", actualCompiledPattern.getPattern());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern2() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act
    Pattern actualCompiledPattern = jakartaOroMatcher.getCompiledPattern(1);

    // Assert
    assertEquals(0, actualCompiledPattern.getOptions());
    assertEquals("", actualCompiledPattern.getPattern());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern3() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act
    Pattern actualCompiledPattern = jakartaOroMatcher.getCompiledPattern(RegexpMatcher.MATCH_CASE_INSENSITIVE);

    // Assert
    assertEquals(1, actualCompiledPattern.getOptions());
    assertEquals("Pattern", actualCompiledPattern.getPattern());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern4() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act
    Pattern actualCompiledPattern = jakartaOroMatcher.getCompiledPattern(RegexpMatcher.MATCH_MULTILINE);

    // Assert
    assertEquals(8, actualCompiledPattern.getOptions());
    assertEquals("Pattern", actualCompiledPattern.getPattern());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getCompiledPattern(int)}
   */
  @Test
  public void testGetCompiledPattern5() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act
    Pattern actualCompiledPattern = jakartaOroMatcher.getCompiledPattern(RegexpMatcher.MATCH_SINGLELINE);

    // Assert
    assertEquals(Regexp.REPLACE_ALL, actualCompiledPattern.getOptions());
    assertEquals("Pattern", actualCompiledPattern.getPattern());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getCompilerOptions(int)}
   */
  @Test
  public void testGetCompilerOptions() {
    // Arrange, Act and Assert
    assertEquals(0, (new JakartaOroMatcher()).getCompilerOptions(1));
    assertEquals(1, (new JakartaOroMatcher()).getCompilerOptions(RegexpMatcher.MATCH_CASE_INSENSITIVE));
    assertEquals(8, (new JakartaOroMatcher()).getCompilerOptions(RegexpMatcher.MATCH_MULTILINE));
    assertEquals(Regexp.REPLACE_ALL, (new JakartaOroMatcher()).getCompilerOptions(RegexpMatcher.MATCH_SINGLELINE));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups("Argument"));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups2() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jakartaOroMatcher.getGroups("Argument");

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups3() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups(""));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups4() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jakartaOroMatcher.getGroups("");

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups5() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42");

    // Act
    Vector<String> actualGroups = jakartaOroMatcher.getGroups("42");

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("42", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups6() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42iogmsx-");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups("42"));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups7() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42");

    // Act
    Vector<String> actualGroups = jakartaOroMatcher.getGroups("42iogmsx-");

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("42", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String)}
   */
  @Test
  public void testGetGroups8() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42iogmsx-");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups("42Argument"));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups9() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups("Input", 1));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups10() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jakartaOroMatcher.getGroups("Input", 1);

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups11() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups("", 1));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups12() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups13() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups("Input", RegexpMatcher.MATCH_MULTILINE));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups14() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertNull(jakartaOroMatcher.getGroups("Input", RegexpMatcher.MATCH_SINGLELINE));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups15() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act
    Vector<String> actualGroups = jakartaOroMatcher.getGroups("", 1);

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#getGroups(String, int)}
   */
  @Test
  public void testGetGroups16() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42");

    // Act
    Vector<String> actualGroups = jakartaOroMatcher.getGroups("42", 1);

    // Assert
    assertEquals(1, actualGroups.size());
    assertEquals("42", actualGroups.get(0));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String)}
   */
  @Test
  public void testMatches() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches("Argument"));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String)}
   */
  @Test
  public void testMatches2() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act and Assert
    assertTrue(jakartaOroMatcher.matches("Argument"));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String)}
   */
  @Test
  public void testMatches3() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches(""));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String)}
   */
  @Test
  public void testMatches4() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act and Assert
    assertTrue(jakartaOroMatcher.matches(""));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String)}
   */
  @Test
  public void testMatches5() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42");

    // Act and Assert
    assertTrue(jakartaOroMatcher.matches("42"));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String)}
   */
  @Test
  public void testMatches6() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42iogmsx-");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches("42"));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String)}
   */
  @Test
  public void testMatches7() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42");

    // Act and Assert
    assertTrue(jakartaOroMatcher.matches("42iogmsx-"));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String)}
   */
  @Test
  public void testMatches8() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42iogmsx-");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches("42Argument"));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String, int)}
   */
  @Test
  public void testMatches9() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches("Input", 1));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String, int)}
   */
  @Test
  public void testMatches10() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act and Assert
    assertTrue(jakartaOroMatcher.matches("Input", 1));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String, int)}
   */
  @Test
  public void testMatches11() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches("", 1));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String, int)}
   */
  @Test
  public void testMatches12() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches("Input", RegexpMatcher.MATCH_CASE_INSENSITIVE));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String, int)}
   */
  @Test
  public void testMatches13() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches("Input", RegexpMatcher.MATCH_MULTILINE));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String, int)}
   */
  @Test
  public void testMatches14() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("Pattern");

    // Act and Assert
    assertFalse(jakartaOroMatcher.matches("Input", RegexpMatcher.MATCH_SINGLELINE));
    assertNull(jakartaOroMatcher.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String, int)}
   */
  @Test
  public void testMatches15() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("");

    // Act and Assert
    assertTrue(jakartaOroMatcher.matches("", 1));
  }

  /**
   * Method under test: {@link JakartaOroMatcher#matches(String, int)}
   */
  @Test
  public void testMatches16() throws BuildException {
    // Arrange
    JakartaOroMatcher jakartaOroMatcher = new JakartaOroMatcher();
    jakartaOroMatcher.setPattern("42");

    // Act and Assert
    assertTrue(jakartaOroMatcher.matches("42", 1));
  }
}

