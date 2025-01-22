package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Iterator;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.PatternSet;
import org.apache.tools.ant.types.PatternSet.NameEntry;
import org.apache.tools.ant.types.PatternSet.PatternFileNameEntry;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.selectors.OrSelector;
import org.apache.tools.ant.types.selectors.ReadableSelector;
import org.apache.tools.ant.types.selectors.SelectSelector;
import org.junit.Test;

public class FilesDiffblueTest {
  /**
   * Test {@link Files#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code null}.</li>
   *   <li>Then {@link Files#Files()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenFilesAppendExcludesArrayOfStringWithNull_thenFilesReference() throws BuildException {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{null});
    Reference r = new Reference("42");

    // Act
    files.setRefid(r);

    // Assert
    assertTrue(files.isReference());
    assertSame(r, files.getRefid());
  }

  /**
   * Test {@link Files#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenFilesAppendIncludesArrayOfStringWithEmptyString() throws BuildException {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{""});
    Reference r = new Reference("42");

    // Act
    files.setRefid(r);

    // Assert
    assertTrue(files.isReference());
    assertSame(r, files.getRefid());
  }

  /**
   * Test {@link Files#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code null}.</li>
   *   <li>Then {@link Files#Files()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenFilesAppendIncludesArrayOfStringWithNull_thenFilesReference() throws BuildException {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{null});
    Reference r = new Reference("42");

    // Act
    files.setRefid(r);

    // Assert
    assertTrue(files.isReference());
    assertSame(r, files.getRefid());
  }

  /**
   * Test {@link Files#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   *   <li>Then {@link Files#Files()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenFiles_thenFilesReference() throws BuildException {
    // Arrange
    Files files = new Files();
    Reference r = new Reference("42");

    // Act
    files.setRefid(r);

    // Assert
    assertTrue(files.isReference());
    assertSame(r, files.getRefid());
  }

  /**
   * Test {@link Files#createPatternSet()}.
   * <p>
   * Method under test: {@link Files#createPatternSet()}
   */
  @Test
  public void testCreatePatternSet() {
    // Arrange and Act
    PatternSet actualCreatePatternSetResult = (new Files()).createPatternSet();

    // Assert
    Location location = actualCreatePatternSetResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePatternSetResult.getDescription());
    assertNull(actualCreatePatternSetResult.getProject());
    assertNull(actualCreatePatternSetResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreatePatternSetResult.isReference());
  }

  /**
   * Test {@link Files#createInclude()}.
   * <p>
   * Method under test: {@link Files#createInclude()}
   */
  @Test
  public void testCreateInclude() {
    // Arrange, Act and Assert
    assertNull((new Files()).createInclude().getName());
  }

  /**
   * Test {@link Files#createIncludesFile()}.
   * <p>
   * Method under test: {@link Files#createIncludesFile()}
   */
  @Test
  public void testCreateIncludesFile() {
    // Arrange and Act
    NameEntry actualCreateIncludesFileResult = (new Files()).createIncludesFile();

    // Assert
    assertTrue(actualCreateIncludesFileResult instanceof PatternFileNameEntry);
    assertNull(actualCreateIncludesFileResult.getName());
    assertNull(((PatternFileNameEntry) actualCreateIncludesFileResult).getEncoding());
  }

  /**
   * Test {@link Files#createExclude()}.
   * <p>
   * Method under test: {@link Files#createExclude()}
   */
  @Test
  public void testCreateExclude() {
    // Arrange, Act and Assert
    assertNull((new Files()).createExclude().getName());
  }

  /**
   * Test {@link Files#createExcludesFile()}.
   * <p>
   * Method under test: {@link Files#createExcludesFile()}
   */
  @Test
  public void testCreateExcludesFile() {
    // Arrange and Act
    NameEntry actualCreateExcludesFileResult = (new Files()).createExcludesFile();

    // Assert
    assertTrue(actualCreateExcludesFileResult instanceof PatternFileNameEntry);
    assertNull(actualCreateExcludesFileResult.getName());
    assertNull(((PatternFileNameEntry) actualCreateExcludesFileResult).getEncoding());
  }

  /**
   * Test {@link Files#setIncludes(String)}.
   * <ul>
   *   <li>When {@code Includes}.</li>
   *   <li>Then {@link Files#Files()} size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#setIncludes(String)}
   */
  @Test
  public void testSetIncludes_whenIncludes_thenFilesSizeIsZero() {
    // Arrange
    Files files = new Files();

    // Act
    files.setIncludes("Includes");

    // Assert
    assertEquals(0, files.size());
    assertTrue(files.isEmpty());
  }

  /**
   * Test {@link Files#appendIncludes(String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code Includes}.</li>
   *   <li>Then {@link Files#Files()} size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#appendIncludes(String[])}
   */
  @Test
  public void testAppendIncludes_whenArrayOfStringWithIncludes_thenFilesSizeIsZero() {
    // Arrange
    Files files = new Files();

    // Act
    files.appendIncludes(new String[]{"Includes"});

    // Assert
    assertEquals(0, files.size());
    assertTrue(files.isEmpty());
  }

  /**
   * Test {@link Files#getDefaultexcludes()}.
   * <p>
   * Method under test: {@link Files#getDefaultexcludes()}
   */
  @Test
  public void testGetDefaultexcludes() {
    // Arrange, Act and Assert
    assertTrue((new Files()).getDefaultexcludes());
  }

  /**
   * Test {@link Files#isCaseSensitive()}.
   * <p>
   * Method under test: {@link Files#isCaseSensitive()}
   */
  @Test
  public void testIsCaseSensitive() {
    // Arrange, Act and Assert
    assertTrue((new Files()).isCaseSensitive());
  }

  /**
   * Test {@link Files#isFollowSymlinks()}.
   * <p>
   * Method under test: {@link Files#isFollowSymlinks()}
   */
  @Test
  public void testIsFollowSymlinks() {
    // Arrange, Act and Assert
    assertTrue((new Files()).isFollowSymlinks());
  }

  /**
   * Test {@link Files#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()} addOr {@link OrSelector} (default constructor).</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#iterator()}
   */
  @Test
  public void testIterator_givenFilesAddOrOrSelector_thenReturnNotHasNext() {
    // Arrange
    Files files = new Files();
    files.addOr(new OrSelector());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertFalse(files.iterator().hasNext());
  }

  /**
   * Test {@link Files#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()} addReadable {@link ReadableSelector} (default constructor).</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#iterator()}
   */
  @Test
  public void testIterator_givenFilesAddReadableReadableSelector_thenReturnNotHasNext() {
    // Arrange
    Files files = new Files();
    files.addReadable(new ReadableSelector());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertFalse(files.iterator().hasNext());
  }

  /**
   * Test {@link Files#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code **}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#iterator()}
   */
  @Test
  public void testIterator_givenFilesAppendExcludesArrayOfStringWithAsteriskAsterisk() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"**"});
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertFalse(files.iterator().hasNext());
  }

  /**
   * Test {@link Files#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#iterator()}
   */
  @Test
  public void testIterator_givenFilesAppendIncludesArrayOfStringWithAsterisk() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"*"});

    // Act and Assert
    assertFalse(files.iterator().hasNext());
  }

  /**
   * Test {@link Files#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#iterator()}
   */
  @Test
  public void testIterator_givenFilesAppendIncludesArrayOfStringWithEmptyString() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{""});

    // Act and Assert
    assertFalse(files.iterator().hasNext());
  }

  /**
   * Test {@link Files#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code .git}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#iterator()}
   */
  @Test
  public void testIterator_givenFilesAppendIncludesArrayOfStringWithGit_thenReturnNotHasNext() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{".git"});

    // Act and Assert
    assertFalse(files.iterator().hasNext());
  }

  /**
   * Test {@link Files#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return next Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#iterator()}
   */
  @Test
  public void testIterator_givenFilesProjectIsProject_thenReturnNextProjectIsProject() {
    // Arrange
    Files files = new Files();
    Project project = new Project();
    files.setProject(project);
    files.appendIncludes(new String[]{"**"});

    // Act
    Iterator<Resource> actualIteratorResult = files.iterator();

    // Assert
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof FileResource);
    assertTrue(actualIteratorResult instanceof FileResourceIterator);
    assertFalse(actualIteratorResult.hasNext());
    assertSame(project, nextResult.getProject());
  }

  /**
   * Test {@link Files#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#iterator()}
   */
  @Test
  public void testIterator_givenFiles_thenReturnNotHasNext() {
    // Arrange, Act and Assert
    assertFalse((new Files()).iterator().hasNext());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} addOr {@link OrSelector} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAddOrOrSelector_thenReturnZero() {
    // Arrange
    Files files = new Files();
    files.addOr(new OrSelector());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(0, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} addReadable {@link ReadableSelector} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAddReadableReadableSelector_thenReturnZero() {
    // Arrange
    Files files = new Files();
    files.addReadable(new ReadableSelector());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(0, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} addSelector {@link SelectSelector} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAddSelectorSelectSelector_thenReturnOne() {
    // Arrange
    Files files = new Files();
    files.addSelector(new SelectSelector());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(1, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code **}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAppendExcludesArrayOfStringWithAsteriskAsterisk() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"**"});
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(0, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code *}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAppendExcludesArrayOfStringWithAsterisk_thenReturnOne() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"*"});
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(1, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code **}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAppendIncludesArrayOfStringWithAsteriskAsterisk_thenReturnOne() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(1, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code *}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAppendIncludesArrayOfStringWithAsterisk_thenReturnOne() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"*"});
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(1, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code *}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAppendIncludesArrayOfStringWithAsterisk_thenReturnZero() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"*"});

    // Act and Assert
    assertEquals(0, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with empty string.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAppendIncludesArrayOfStringWithEmptyString_thenReturnZero() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{""});

    // Act and Assert
    assertEquals(0, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code .git}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesAppendIncludesArrayOfStringWithGit_thenReturnZero() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{".git"});

    // Act and Assert
    assertEquals(0, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFilesProjectIsProject_thenReturnOne() {
    // Arrange
    Files files = new Files();
    files.setProject(new Project());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(1, files.size());
  }

  /**
   * Test {@link Files#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#size()}
   */
  @Test
  public void testSize_givenFiles_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Files()).size());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code Excludes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesAppendExcludesArrayOfStringWithExcludes_thenReturnTrue() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"Excludes"});

    // Act and Assert
    assertTrue(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesAppendExcludesArrayOfStringWithNull_thenReturnFalse() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{null});

    // Act and Assert
    assertFalse(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesAppendIncludesArrayOfStringWithEmptyString() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{""});

    // Act and Assert
    assertFalse(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code Includes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesAppendIncludesArrayOfStringWithIncludes_thenReturnTrue() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertTrue(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code Includes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesAppendIncludesArrayOfStringWithIncludes_thenReturnTrue2() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertTrue(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesAppendIncludesArrayOfStringWithNull_thenReturnFalse() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{null});

    // Act and Assert
    assertFalse(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesAppendSelectorScriptSelector_thenReturnTrue() {
    // Arrange
    Files files = new Files();
    files.appendSelector(new ScriptSelector());
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertTrue(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesProjectIsProject_thenReturnTrue() {
    // Arrange
    Files files = new Files();
    files.setProject(new Project());
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertTrue(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFilesProjectIsProject_thenReturnTrue2() {
    // Arrange
    Files files = new Files();
    files.setProject(new Project());
    files.appendIncludes(new String[]{"Includes"});
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertTrue(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenFiles_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Files()).hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenProjectAddBuildListenerAntClassLoader_thenReturnTrue() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Files files = new Files();
    files.setProject(project);
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertTrue(files.hasPatterns());
  }

  /**
   * Test {@link Files#hasPatterns()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#hasPatterns()}
   */
  @Test
  public void testHasPatterns_givenProjectAddTargetAddingReferenceAndTarget_thenReturnTrue() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    Files files = new Files();
    files.setProject(project);
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertTrue(files.hasPatterns());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} addOr {@link OrSelector} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAddOrOrSelector_thenReturnEmptyString() {
    // Arrange
    Files files = new Files();
    files.addOr(new OrSelector());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals("", files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} addReadable {@link ReadableSelector} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAddReadableReadableSelector_thenReturnEmptyString() {
    // Arrange
    Files files = new Files();
    files.addReadable(new ReadableSelector());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals("", files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} addSelector {@link SelectSelector} (default constructor).</li>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAddSelectorSelectSelector_thenReturnPropertyIsUserDir() {
    // Arrange
    Files files = new Files();
    files.addSelector(new SelectSelector());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAppendExcludesArrayOfStringWithAsterisk() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"*"});
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code **}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAppendExcludesArrayOfStringWithAsteriskAsterisk() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"**"});
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals("", files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAppendIncludesArrayOfStringWithAsterisk() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"*"});
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAppendIncludesArrayOfStringWithAsterisk2() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"*"});

    // Act and Assert
    assertEquals("", files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAppendIncludesArrayOfStringWithEmptyString() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{""});

    // Act and Assert
    assertEquals("", files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code .git}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesAppendIncludesArrayOfStringWithGit_thenReturnEmptyString() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{".git"});

    // Act and Assert
    assertEquals("", files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFilesProjectIsProject_thenReturnPropertyIsUserDir() {
    // Arrange
    Files files = new Files();
    files.setProject(new Project());
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), files.toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_givenFiles_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new Files()).toString());
  }

  /**
   * Test {@link Files#toString()}.
   * <ul>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#toString()}
   */
  @Test
  public void testToString_thenReturnPropertyIsUserDir() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"**"});

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), files.toString());
  }

  /**
   * Test {@link Files#clone()}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#clone()}
   */
  @Test
  public void testClone_givenFiles() {
    // Arrange and Act
    Object actualCloneResult = (new Files()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof Files);
    Location location = ((Files) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Files) actualCloneResult).getDescription());
    assertNull(((Files) actualCloneResult).getProject());
    assertNull(((Files) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(((Files) actualCloneResult).isReference());
    assertTrue(((Files) actualCloneResult).getDefaultexcludes());
    assertTrue(((Files) actualCloneResult).isCaseSensitive());
    assertTrue(((Files) actualCloneResult).isFilesystemOnly());
    assertTrue(((Files) actualCloneResult).isFollowSymlinks());
  }

  /**
   * Test {@link Files#clone()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#clone()}
   */
  @Test
  public void testClone_givenFilesAppendSelectorScriptSelector_thenReturnEmpty() {
    // Arrange
    Files files = new Files();
    files.appendSelector(new ScriptSelector());

    // Act
    Object actualCloneResult = files.clone();

    // Assert
    assertTrue(actualCloneResult instanceof Files);
    Location location = ((Files) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Files) actualCloneResult).getDescription());
    assertNull(((Files) actualCloneResult).getProject());
    assertNull(((Files) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(((Files) actualCloneResult).isReference());
    assertTrue(((Files) actualCloneResult).isEmpty());
    assertTrue(((Files) actualCloneResult).getDefaultexcludes());
    assertTrue(((Files) actualCloneResult).isCaseSensitive());
    assertTrue(((Files) actualCloneResult).isFilesystemOnly());
    assertTrue(((Files) actualCloneResult).isFollowSymlinks());
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>Given {@code Adding reference:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_givenAddingReference() throws BuildException {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());

    // Act and Assert
    assertArrayEquals(new String[]{"Includes"}, files.mergeIncludes(p));
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertArrayEquals(new String[]{"Includes"}, files.mergeIncludes(p));
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code ant.PropertyHelper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_givenFilesAppendExcludesArrayOfStringWithAntPropertyHelper() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"ant.PropertyHelper"});
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertArrayEquals(new String[]{"Includes"}, files.mergeIncludes(new Project()));
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_givenFilesAppendIncludesArrayOfStringWithEmptyString() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{""});

    // Act and Assert
    assertNull(files.mergeIncludes(new Project()));
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_givenFilesAppendSelectorScriptSelector() {
    // Arrange
    Files files = new Files();
    files.appendSelector(new ScriptSelector());
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertArrayEquals(new String[]{"Includes"}, files.mergeIncludes(new Project()));
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_givenFiles_whenProject_thenReturnNull() {
    // Arrange
    Files files = new Files();

    // Act and Assert
    assertNull(files.mergeIncludes(new Project()));
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code ant.PropertyHelper} and {@code Includes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_thenReturnArrayOfStringWithAntPropertyHelperAndIncludes() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"ant.PropertyHelper"});
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertArrayEquals(new String[]{"ant.PropertyHelper", "Includes"}, files.mergeIncludes(new Project()));
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return array of {@link String} with {@code Includes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_whenNull_thenReturnArrayOfStringWithIncludes() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertArrayEquals(new String[]{"Includes"}, files.mergeIncludes(null));
  }

  /**
   * Test {@link Files#mergeIncludes(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return array of {@link String} with {@code Includes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeIncludes(Project)}
   */
  @Test
  public void testMergeIncludes_whenProject_thenReturnArrayOfStringWithIncludes() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertArrayEquals(new String[]{"Includes"}, files.mergeIncludes(new Project()));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Given {@code Adding reference:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_givenAddingReference() throws BuildException {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());

    // Act and Assert
    assertNull(files.mergeExcludes(p));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(files.mergeExcludes(p));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code ant.PropertyHelper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_givenFilesAppendIncludesArrayOfStringWithAntPropertyHelper() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"ant.PropertyHelper"});
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertNull(files.mergeExcludes(new Project()));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_givenFilesAppendIncludesArrayOfStringWithEmptyString() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{""});

    // Act and Assert
    assertNull(files.mergeExcludes(new Project()));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code Includes}.</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_givenFilesAppendIncludesArrayOfStringWithIncludes_whenNull() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertNull(files.mergeExcludes(null));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code Includes}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_givenFilesAppendIncludesArrayOfStringWithIncludes_whenProject() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertNull(files.mergeExcludes(new Project()));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_givenFilesAppendSelectorScriptSelector_whenProject() {
    // Arrange
    Files files = new Files();
    files.appendSelector(new ScriptSelector());
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertNull(files.mergeExcludes(new Project()));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_givenFiles_whenProject_thenReturnNull() {
    // Arrange
    Files files = new Files();

    // Act and Assert
    assertNull(files.mergeExcludes(new Project()));
  }

  /**
   * Test {@link Files#mergeExcludes(Project)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code ant.PropertyHelper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergeExcludes(Project)}
   */
  @Test
  public void testMergeExcludes_thenReturnArrayOfStringWithAntPropertyHelper() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"ant.PropertyHelper"});
    files.appendIncludes(new String[]{"Includes"});

    // Act and Assert
    assertArrayEquals(new String[]{"ant.PropertyHelper"}, files.mergeExcludes(new Project()));
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@code Adding reference:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenAddingReference() throws BuildException {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    Project p = new Project();
    p.addTarget("Adding reference: ", new Target());

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(p);

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(p);

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendExcludes array of {@link String} with {@code ant.PropertyHelper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenFilesAppendExcludesArrayOfStringWithAntPropertyHelper() {
    // Arrange
    Files files = new Files();
    files.appendExcludes(new String[]{"ant.PropertyHelper"});
    files.appendIncludes(new String[]{"Includes"});

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(new Project());

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code ant.PropertyHelper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenFilesAppendIncludesArrayOfStringWithAntPropertyHelper() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"ant.PropertyHelper"});
    files.appendIncludes(new String[]{"Includes"});

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(new Project());

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenFilesAppendIncludesArrayOfStringWithEmptyString() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{""});

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(new Project());

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code Includes}.</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenFilesAppendIncludesArrayOfStringWithIncludes_whenNull() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(null);

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code Includes}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenFilesAppendIncludesArrayOfStringWithIncludes_whenProject() {
    // Arrange
    Files files = new Files();
    files.appendIncludes(new String[]{"Includes"});

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(new Project());

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenFilesAppendSelectorScriptSelector_whenProject() {
    // Arrange
    Files files = new Files();
    files.appendSelector(new ScriptSelector());
    files.appendIncludes(new String[]{"Includes"});

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(new Project());

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#mergePatterns(Project)}.
   * <ul>
   *   <li>Given {@link Files#Files()}.</li>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Files#mergePatterns(Project)}
   */
  @Test
  public void testMergePatterns_givenFiles_whenProject() {
    // Arrange
    Files files = new Files();

    // Act
    PatternSet actualMergePatternsResult = files.mergePatterns(new Project());

    // Assert
    Location location = actualMergePatternsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMergePatternsResult.getDescription());
    assertNull(actualMergePatternsResult.getProject());
    assertNull(actualMergePatternsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMergePatternsResult.isReference());
  }

  /**
   * Test {@link Files#isFilesystemOnly()}.
   * <p>
   * Method under test: {@link Files#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly() {
    // Arrange, Act and Assert
    assertTrue((new Files()).isFilesystemOnly());
  }
}
