package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.PatternSet.NameEntry;
import org.apache.tools.ant.types.PatternSet.PatternFileNameEntry;
import org.junit.Test;

public class PatternSetDiffblueTest {
  /**
   * Test NameEntry getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link NameEntry#NameEntry(PatternSet)}
   *   <li>{@link NameEntry#setIf(Object)}
   *   <li>{@link NameEntry#setName(String)}
   *   <li>{@link NameEntry#setUnless(Object)}
   *   <li>{@link NameEntry#getName()}
   * </ul>
   */
  @Test
  public void testNameEntryGettersAndSetters() {
    // Arrange and Act
    NameEntry actualNameEntry = (new PatternSet()).new NameEntry();
    actualNameEntry.setIf((Object) "Cond");
    actualNameEntry.setName("Name");
    actualNameEntry.setUnless((Object) "Cond");

    // Assert
    assertEquals("Name", actualNameEntry.getName());
  }

  /**
   * Test new {@link PatternSet} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link PatternSet}
   */
  @Test
  public void testNewPatternSet() {
    // Arrange and Act
    PatternSet actualPatternSet = new PatternSet();

    // Assert
    assertEquals("PatternSet", actualPatternSet.getDataTypeName());
    Location location = actualPatternSet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPatternSet.getDescription());
    assertNull(actualPatternSet.getProject());
    assertNull(actualPatternSet.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualPatternSet.isReference());
    assertTrue(actualPatternSet.isChecked());
  }

  /**
   * Test PatternFileNameEntry getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PatternFileNameEntry#PatternFileNameEntry(PatternSet)}
   *   <li>{@link PatternFileNameEntry#setEncoding(String)}
   *   <li>{@link PatternFileNameEntry#getEncoding()}
   * </ul>
   */
  @Test
  public void testPatternFileNameEntryGettersAndSetters() {
    // Arrange and Act
    PatternFileNameEntry actualPatternFileNameEntry = (new PatternSet()).new PatternFileNameEntry();
    actualPatternFileNameEntry.setEncoding("UTF-8");

    // Assert
    assertEquals("UTF-8", actualPatternFileNameEntry.getEncoding());
    assertNull(actualPatternFileNameEntry.getName());
  }

  /**
   * Test PatternFileNameEntry {@link PatternFileNameEntry#toString()}.
   * <ul>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternFileNameEntry#toString()}
   */
  @Test
  public void testPatternFileNameEntryToString_thenReturnFoo() {
    // Arrange
    PatternFileNameEntry patternFileNameEntry = (new PatternSet()).new PatternFileNameEntry();
    patternFileNameEntry.setName("foo");
    patternFileNameEntry.setIf((Object) null);
    patternFileNameEntry.setUnless((Object) null);

    // Act and Assert
    assertEquals("foo", patternFileNameEntry.toString());
  }

  /**
   * Test PatternFileNameEntry {@link PatternFileNameEntry#toString()}.
   * <ul>
   *   <li>Then return {@code noname}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternFileNameEntry#toString()}
   */
  @Test
  public void testPatternFileNameEntryToString_thenReturnNoname() {
    // Arrange, Act and Assert
    assertEquals("noname", ((new PatternSet()).new PatternFileNameEntry()).toString());
  }

  /**
   * Test PatternFileNameEntry {@link PatternFileNameEntry#toString()}.
   * <ul>
   *   <li>Then return {@code noname;encoding->foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternFileNameEntry#toString()}
   */
  @Test
  public void testPatternFileNameEntryToString_thenReturnNonameEncodingFoo() {
    // Arrange
    PatternFileNameEntry patternFileNameEntry = (new PatternSet()).new PatternFileNameEntry();
    patternFileNameEntry.setEncoding("foo");

    // Act and Assert
    assertEquals("noname;encoding->foo", patternFileNameEntry.toString());
  }

  /**
   * Test PatternFileNameEntry {@link PatternFileNameEntry#toString()}.
   * <ul>
   *   <li>Then return {@code noname:if->42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternFileNameEntry#toString()}
   */
  @Test
  public void testPatternFileNameEntryToString_thenReturnNonameIf42() {
    // Arrange
    PatternFileNameEntry patternFileNameEntry = (new PatternSet()).new PatternFileNameEntry();
    patternFileNameEntry.setName(null);
    patternFileNameEntry.setIf((Object) "42");
    patternFileNameEntry.setUnless((Object) null);

    // Act and Assert
    assertEquals("noname:if->42", patternFileNameEntry.toString());
  }

  /**
   * Test PatternFileNameEntry {@link PatternFileNameEntry#toString()}.
   * <ul>
   *   <li>Then return {@code noname:unless->42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternFileNameEntry#toString()}
   */
  @Test
  public void testPatternFileNameEntryToString_thenReturnNonameUnless42() {
    // Arrange
    PatternFileNameEntry patternFileNameEntry = (new PatternSet()).new PatternFileNameEntry();
    patternFileNameEntry.setName(null);
    patternFileNameEntry.setIf((Object) null);
    patternFileNameEntry.setUnless((Object) "42");

    // Act and Assert
    assertEquals("noname:unless->42", patternFileNameEntry.toString());
  }

  /**
   * Test {@link PatternSet#setRefid(Reference)}.
   * <p>
   * Method under test: {@link PatternSet#setRefid(Reference)}
   */
  @Test
  public void testSetRefid() throws BuildException {
    // Arrange
    PatternSet patternSet = new PatternSet();
    Reference r = new Reference("42");

    // Act
    patternSet.setRefid(r);

    // Assert
    assertFalse(patternSet.isChecked());
    assertTrue(patternSet.isReference());
    assertSame(r, patternSet.getRefid());
  }

  /**
   * Test {@link PatternSet#createInclude()}.
   * <p>
   * Method under test: {@link PatternSet#createInclude()}
   */
  @Test
  public void testCreateInclude() {
    // Arrange, Act and Assert
    assertNull((new PatternSet()).createInclude().getName());
  }

  /**
   * Test {@link PatternSet#createIncludesFile()}.
   * <p>
   * Method under test: {@link PatternSet#createIncludesFile()}
   */
  @Test
  public void testCreateIncludesFile() {
    // Arrange and Act
    NameEntry actualCreateIncludesFileResult = (new PatternSet()).createIncludesFile();

    // Assert
    assertTrue(actualCreateIncludesFileResult instanceof PatternFileNameEntry);
    assertNull(actualCreateIncludesFileResult.getName());
    assertNull(((PatternFileNameEntry) actualCreateIncludesFileResult).getEncoding());
  }

  /**
   * Test {@link PatternSet#createExclude()}.
   * <p>
   * Method under test: {@link PatternSet#createExclude()}
   */
  @Test
  public void testCreateExclude() {
    // Arrange, Act and Assert
    assertNull((new PatternSet()).createExclude().getName());
  }

  /**
   * Test {@link PatternSet#createExcludesFile()}.
   * <p>
   * Method under test: {@link PatternSet#createExcludesFile()}
   */
  @Test
  public void testCreateExcludesFile() {
    // Arrange and Act
    NameEntry actualCreateExcludesFileResult = (new PatternSet()).createExcludesFile();

    // Assert
    assertTrue(actualCreateExcludesFileResult instanceof PatternFileNameEntry);
    assertNull(actualCreateExcludesFileResult.getName());
    assertNull(((PatternFileNameEntry) actualCreateExcludesFileResult).getEncoding());
  }

  /**
   * Test {@link PatternSet#getIncludePatterns(Project)}.
   * <p>
   * Method under test: {@link PatternSet#getIncludePatterns(Project)}
   */
  @Test
  public void testGetIncludePatterns() {
    // Arrange
    PatternSet patternSet = new PatternSet();

    // Act and Assert
    assertNull(patternSet.getIncludePatterns(new Project()));
  }

  /**
   * Test {@link PatternSet#getExcludePatterns(Project)}.
   * <p>
   * Method under test: {@link PatternSet#getExcludePatterns(Project)}
   */
  @Test
  public void testGetExcludePatterns() {
    // Arrange
    PatternSet patternSet = new PatternSet();

    // Act and Assert
    assertNull(patternSet.getExcludePatterns(new Project()));
  }

  /**
   * Test {@link PatternSet#hasPatterns(Project)}.
   * <p>
   * Method under test: {@link PatternSet#hasPatterns(Project)}
   */
  @Test
  public void testHasPatterns() {
    // Arrange
    PatternSet patternSet = new PatternSet();

    // Act and Assert
    assertFalse(patternSet.hasPatterns(new Project()));
  }

  /**
   * Test {@link PatternSet#toString()}.
   * <p>
   * Method under test: {@link PatternSet#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("patternSet{ includes: [] excludes: [] }", (new PatternSet()).toString());
  }

  /**
   * Test {@link PatternSet#clone()}.
   * <ul>
   *   <li>Given {@link PatternSet} (default constructor) Checked is {@code false}.</li>
   *   <li>Then return not Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternSet#clone()}
   */
  @Test
  public void testClone_givenPatternSetCheckedIsFalse_thenReturnNotChecked() {
    // Arrange
    PatternSet patternSet = new PatternSet();
    patternSet.setChecked(false);

    // Act
    Object actualCloneResult = patternSet.clone();

    // Assert
    assertTrue(actualCloneResult instanceof PatternSet);
    assertEquals("PatternSet", ((PatternSet) actualCloneResult).getDataTypeName());
    Location location = ((PatternSet) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((PatternSet) actualCloneResult).getDescription());
    assertNull(((PatternSet) actualCloneResult).getProject());
    assertNull(((PatternSet) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(((PatternSet) actualCloneResult).isChecked());
    assertFalse(((PatternSet) actualCloneResult).isReference());
  }

  /**
   * Test {@link PatternSet#clone()}.
   * <ul>
   *   <li>Given {@link PatternSet} (default constructor).</li>
   *   <li>Then return Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternSet#clone()}
   */
  @Test
  public void testClone_givenPatternSet_thenReturnChecked() {
    // Arrange and Act
    Object actualCloneResult = (new PatternSet()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof PatternSet);
    assertEquals("PatternSet", ((PatternSet) actualCloneResult).getDataTypeName());
    Location location = ((PatternSet) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((PatternSet) actualCloneResult).getDescription());
    assertNull(((PatternSet) actualCloneResult).getProject());
    assertNull(((PatternSet) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(((PatternSet) actualCloneResult).isReference());
    assertTrue(((PatternSet) actualCloneResult).isChecked());
  }
}
