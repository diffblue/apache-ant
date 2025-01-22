package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.RegularExpression;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.Substitution;
import org.apache.tools.ant.types.TarFileSet;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class ReplaceRegExpDiffblueTest {
  /**
   * Test new {@link ReplaceRegExp} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ReplaceRegExp}
   */
  @Test
  public void testNewReplaceRegExp() {
    // Arrange and Act
    ReplaceRegExp actualReplaceRegExp = new ReplaceRegExp();

    // Assert
    Location location = actualReplaceRegExp.getLocation();
    assertNull(location.getFileName());
    assertNull(actualReplaceRegExp.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualReplaceRegExp.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualReplaceRegExp.getTaskName());
    assertNull(actualReplaceRegExp.getTaskType());
    assertNull(actualReplaceRegExp.getProject());
    assertNull(actualReplaceRegExp.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualReplaceRegExp, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link ReplaceRegExp#setMatch(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor) Match is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#setMatch(String)}
   */
  @Test
  public void testSetMatch_givenReplaceRegExpMatchIsNull_thenThrowBuildException() {
    // Arrange
    ReplaceRegExp replaceRegExp = new ReplaceRegExp();
    replaceRegExp.setMatch(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegExp.setMatch("Match"));
  }

  /**
   * Test {@link ReplaceRegExp#setReplace(String)}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor) Replace is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#setReplace(String)}
   */
  @Test
  public void testSetReplace_givenReplaceRegExpReplaceIsNull_thenThrowBuildException() {
    // Arrange
    ReplaceRegExp replaceRegExp = new ReplaceRegExp();
    replaceRegExp.setReplace(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegExp.setReplace("Replace"));
  }

  /**
   * Test {@link ReplaceRegExp#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenResource_thenThrowBuildException() {
    // Arrange
    ReplaceRegExp replaceRegExp = new ReplaceRegExp();

    TarFileSet set = new TarFileSet();
    set.setSrcResource(new Resource());
    set.appendSelector(new ScriptSelector());
    set.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegExp.addFileset(set));
  }

  /**
   * Test {@link ReplaceRegExp#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor).</li>
   *   <li>When {@link Concat} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenReplaceRegExp_whenConcat_thenThrowBuildException() {
    // Arrange
    ReplaceRegExp replaceRegExp = new ReplaceRegExp();

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegExp.addConfigured(new Concat()));
  }

  /**
   * Test {@link ReplaceRegExp#createRegexp()}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor) Match is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#createRegexp()}
   */
  @Test
  public void testCreateRegexp_givenReplaceRegExpMatchIsNull_thenThrowBuildException() {
    // Arrange
    ReplaceRegExp replaceRegExp = new ReplaceRegExp();
    replaceRegExp.setMatch(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegExp.createRegexp());
  }

  /**
   * Test {@link ReplaceRegExp#createRegexp()}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#createRegexp()}
   */
  @Test
  public void testCreateRegexp_givenReplaceRegExp_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    RegularExpression actualCreateRegexpResult = (new ReplaceRegExp()).createRegexp();

    // Assert
    Location location = actualCreateRegexpResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateRegexpResult.getDescription());
    assertNull(actualCreateRegexpResult.getProject());
    assertNull(actualCreateRegexpResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreateRegexpResult.isReference());
  }

  /**
   * Test {@link ReplaceRegExp#createSubstitution()}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor) Replace is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#createSubstitution()}
   */
  @Test
  public void testCreateSubstitution_givenReplaceRegExpReplaceIsNull_thenThrowBuildException() {
    // Arrange
    ReplaceRegExp replaceRegExp = new ReplaceRegExp();
    replaceRegExp.setReplace(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegExp.createSubstitution());
  }

  /**
   * Test {@link ReplaceRegExp#createSubstitution()}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#createSubstitution()}
   */
  @Test
  public void testCreateSubstitution_givenReplaceRegExp_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Substitution actualCreateSubstitutionResult = (new ReplaceRegExp()).createSubstitution();

    // Assert
    Location location = actualCreateSubstitutionResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateSubstitutionResult.getDescription());
    assertNull(actualCreateSubstitutionResult.getProject());
    assertNull(actualCreateSubstitutionResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreateSubstitutionResult.isReference());
  }

  /**
   * Test {@link ReplaceRegExp#execute()}.
   * <p>
   * Method under test: {@link ReplaceRegExp#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    ReplaceRegExp replaceRegExp = new ReplaceRegExp();
    replaceRegExp.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    replaceRegExp.addFileset(new FileSet());
    replaceRegExp.setReplace("Nothing to replace expression with.");
    replaceRegExp.setMatch("No expression to match.");

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegExp.execute());
  }

  /**
   * Test {@link ReplaceRegExp#execute()}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor) Match is {@code No expression to match.}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#execute()}
   */
  @Test
  public void testExecute_givenReplaceRegExpMatchIsNoExpressionToMatch_thenThrowBuildException() throws BuildException {
    // Arrange
    ReplaceRegExp replaceRegExp = new ReplaceRegExp();
    replaceRegExp.setMatch("No expression to match.");

    // Act and Assert
    assertThrows(BuildException.class, () -> replaceRegExp.execute());
  }

  /**
   * Test {@link ReplaceRegExp#execute()}.
   * <ul>
   *   <li>Given {@link ReplaceRegExp} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReplaceRegExp#execute()}
   */
  @Test
  public void testExecute_givenReplaceRegExp_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ReplaceRegExp()).execute());
  }
}
