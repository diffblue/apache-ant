package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Iterator;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.PatternSet;
import org.apache.tools.ant.types.PatternSet.NameEntry;
import org.apache.tools.ant.types.PatternSet.PatternFileNameEntry;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.resources.FileResourceIterator;
import org.apache.tools.ant.types.selectors.AndSelector;
import org.apache.tools.ant.types.selectors.ContainsRegexpSelector;
import org.apache.tools.ant.types.selectors.ContainsSelector;
import org.apache.tools.ant.types.selectors.DateSelector;
import org.apache.tools.ant.types.selectors.DependSelector;
import org.apache.tools.ant.types.selectors.DepthSelector;
import org.apache.tools.ant.types.selectors.DifferentSelector;
import org.apache.tools.ant.types.selectors.ExtendSelector;
import org.apache.tools.ant.types.selectors.FileSelector;
import org.apache.tools.ant.types.selectors.FilenameSelector;
import org.apache.tools.ant.types.selectors.MajoritySelector;
import org.apache.tools.ant.types.selectors.NoneSelector;
import org.apache.tools.ant.types.selectors.NotSelector;
import org.apache.tools.ant.types.selectors.OrSelector;
import org.apache.tools.ant.types.selectors.PresentSelector;
import org.apache.tools.ant.types.selectors.SelectSelector;
import org.apache.tools.ant.types.selectors.SizeSelector;
import org.apache.tools.ant.types.selectors.TypeSelector;
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector;
import org.junit.Test;

public class MatchingTaskDiffblueTest {
  /**
   * Test {@link MatchingTask#setProject(Project)}.
   * <p>
   * Method under test: {@link MatchingTask#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    Checksum checksum = new Checksum();
    Project project = new Project();

    // Act
    checksum.setProject(project);

    // Assert
    assertSame(project, checksum.getProject());
    assertSame(project, checksum.getImplicitFileSet().getProject());
  }

  /**
   * Test {@link MatchingTask#createInclude()}.
   * <p>
   * Method under test: {@link MatchingTask#createInclude()}
   */
  @Test
  public void testCreateInclude() {
    // Arrange, Act and Assert
    assertNull((new Checksum()).createInclude().getName());
  }

  /**
   * Test {@link MatchingTask#createIncludesFile()}.
   * <p>
   * Method under test: {@link MatchingTask#createIncludesFile()}
   */
  @Test
  public void testCreateIncludesFile() {
    // Arrange and Act
    NameEntry actualCreateIncludesFileResult = (new Checksum()).createIncludesFile();

    // Assert
    assertTrue(actualCreateIncludesFileResult instanceof PatternFileNameEntry);
    assertNull(actualCreateIncludesFileResult.getName());
    assertNull(((PatternFileNameEntry) actualCreateIncludesFileResult).getEncoding());
  }

  /**
   * Test {@link MatchingTask#createExclude()}.
   * <p>
   * Method under test: {@link MatchingTask#createExclude()}
   */
  @Test
  public void testCreateExclude() {
    // Arrange, Act and Assert
    assertNull((new Checksum()).createExclude().getName());
  }

  /**
   * Test {@link MatchingTask#createExcludesFile()}.
   * <p>
   * Method under test: {@link MatchingTask#createExcludesFile()}
   */
  @Test
  public void testCreateExcludesFile() {
    // Arrange and Act
    NameEntry actualCreateExcludesFileResult = (new Checksum()).createExcludesFile();

    // Assert
    assertTrue(actualCreateExcludesFileResult instanceof PatternFileNameEntry);
    assertNull(actualCreateExcludesFileResult.getName());
    assertNull(((PatternFileNameEntry) actualCreateExcludesFileResult).getEncoding());
  }

  /**
   * Test {@link MatchingTask#createPatternSet()}.
   * <p>
   * Method under test: {@link MatchingTask#createPatternSet()}
   */
  @Test
  public void testCreatePatternSet() {
    // Arrange and Act
    PatternSet actualCreatePatternSetResult = (new Checksum()).createPatternSet();

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
   * Test {@link MatchingTask#getDirectoryScanner(File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MatchingTask#getDirectoryScanner(File)}
   */
  @Test
  public void testGetDirectoryScanner_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Checksum checksum = new Checksum();
    checksum.setProject(project);
    File baseDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    DirectoryScanner actualDirectoryScanner = checksum.getDirectoryScanner(baseDir);

    // Assert
    FileSet implicitFileSet = checksum.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    assertEquals(0, actualDirectoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, actualDirectoryScanner.getIncludedFiles().length);
    assertEquals(0, actualDirectoryScanner.getNotFollowedSymlinks().length);
    assertEquals(1, actualDirectoryScanner.getIncludedDirsCount());
    assertFalse(iteratorResult.hasNext());
    assertTrue(actualDirectoryScanner.isCaseSensitive());
    assertTrue(actualDirectoryScanner.isEverythingIncluded());
    assertTrue(actualDirectoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
    assertSame(baseDir, actualDirectoryScanner.getBasedir());
    assertSame(baseDir, implicitFileSet.getDir());
    assertArrayEquals(new String[]{""}, actualDirectoryScanner.getIncludedDirectories());
  }

  /**
   * Test {@link MatchingTask#getDirectoryScanner(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MatchingTask#getDirectoryScanner(File)}
   */
  @Test
  public void testGetDirectoryScanner_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Checksum checksum = new Checksum();
    checksum.setProject(project);
    File baseDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    DirectoryScanner actualDirectoryScanner = checksum.getDirectoryScanner(baseDir);

    // Assert
    FileSet implicitFileSet = checksum.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    assertEquals(0, actualDirectoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, actualDirectoryScanner.getIncludedFiles().length);
    assertEquals(0, actualDirectoryScanner.getNotFollowedSymlinks().length);
    assertEquals(1, actualDirectoryScanner.getIncludedDirsCount());
    assertFalse(iteratorResult.hasNext());
    assertTrue(actualDirectoryScanner.isCaseSensitive());
    assertTrue(actualDirectoryScanner.isEverythingIncluded());
    assertTrue(actualDirectoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
    assertSame(baseDir, actualDirectoryScanner.getBasedir());
    assertSame(baseDir, implicitFileSet.getDir());
    assertArrayEquals(new String[]{""}, actualDirectoryScanner.getIncludedDirectories());
  }

  /**
   * Test {@link MatchingTask#getDirectoryScanner(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MatchingTask#getDirectoryScanner(File)}
   */
  @Test
  public void testGetDirectoryScanner_givenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    Checksum checksum = new Checksum();
    checksum.setProject(project);
    File baseDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    DirectoryScanner actualDirectoryScanner = checksum.getDirectoryScanner(baseDir);

    // Assert
    FileSet implicitFileSet = checksum.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    assertEquals(0, actualDirectoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, actualDirectoryScanner.getIncludedFiles().length);
    assertEquals(0, actualDirectoryScanner.getNotFollowedSymlinks().length);
    assertEquals(1, actualDirectoryScanner.getIncludedDirsCount());
    assertFalse(iteratorResult.hasNext());
    assertTrue(actualDirectoryScanner.isCaseSensitive());
    assertTrue(actualDirectoryScanner.isEverythingIncluded());
    assertTrue(actualDirectoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
    assertSame(baseDir, actualDirectoryScanner.getBasedir());
    assertSame(baseDir, implicitFileSet.getDir());
    assertArrayEquals(new String[]{""}, actualDirectoryScanner.getIncludedDirectories());
  }

  /**
   * Test {@link MatchingTask#getDirectoryScanner(File)}.
   * <ul>
   *   <li>Then {@link Checksum} (default constructor) ImplicitFileSet iterator {@link FileResourceIterator}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MatchingTask#getDirectoryScanner(File)}
   */
  @Test
  public void testGetDirectoryScanner_thenChecksumImplicitFileSetIteratorFileResourceIterator() {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.setProject(new Project());
    File baseDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    DirectoryScanner actualDirectoryScanner = checksum.getDirectoryScanner(baseDir);

    // Assert
    FileSet implicitFileSet = checksum.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    assertEquals(0, actualDirectoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, actualDirectoryScanner.getIncludedFiles().length);
    assertEquals(0, actualDirectoryScanner.getNotFollowedSymlinks().length);
    assertEquals(1, actualDirectoryScanner.getIncludedDirsCount());
    assertFalse(iteratorResult.hasNext());
    assertTrue(actualDirectoryScanner.isCaseSensitive());
    assertTrue(actualDirectoryScanner.isEverythingIncluded());
    assertTrue(actualDirectoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
    assertSame(baseDir, actualDirectoryScanner.getBasedir());
    assertSame(baseDir, implicitFileSet.getDir());
    assertArrayEquals(new String[]{""}, actualDirectoryScanner.getIncludedDirectories());
  }

  /**
   * Test {@link MatchingTask#hasSelectors()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor) appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MatchingTask#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenChecksumAppendSelectorScriptSelector_thenReturnTrue() {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.appendSelector(new ScriptSelector());

    // Act and Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#hasSelectors()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MatchingTask#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenChecksum_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Checksum()).hasSelectors());
  }

  /**
   * Test {@link MatchingTask#selectorCount()}.
   * <p>
   * Method under test: {@link MatchingTask#selectorCount()}
   */
  @Test
  public void testSelectorCount() {
    // Arrange, Act and Assert
    assertEquals(0, (new Checksum()).selectorCount());
  }

  /**
   * Test {@link MatchingTask#getSelectors(Project)}.
   * <p>
   * Method under test: {@link MatchingTask#getSelectors(Project)}
   */
  @Test
  public void testGetSelectors() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act and Assert
    assertEquals(0, checksum.getSelectors(new Project()).length);
  }

  /**
   * Test {@link MatchingTask#appendSelector(FileSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#appendSelector(FileSelector)}
   */
  @Test
  public void testAppendSelector() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.appendSelector(new ScriptSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addSelector(SelectSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addSelector(SelectSelector)}
   */
  @Test
  public void testAddSelector() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addSelector(new SelectSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addAnd(AndSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addAnd(AndSelector)}
   */
  @Test
  public void testAddAnd() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addAnd(new AndSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addOr(OrSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addOr(OrSelector)}
   */
  @Test
  public void testAddOr() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addOr(new OrSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addNot(NotSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addNot(NotSelector)}
   */
  @Test
  public void testAddNot() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addNot(new NotSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addNone(NoneSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addNone(NoneSelector)}
   */
  @Test
  public void testAddNone() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addNone(new NoneSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addMajority(MajoritySelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addMajority(MajoritySelector)}
   */
  @Test
  public void testAddMajority() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addMajority(new MajoritySelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addDate(DateSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addDate(DateSelector)}
   */
  @Test
  public void testAddDate() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addDate(new DateSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addSize(SizeSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addSize(SizeSelector)}
   */
  @Test
  public void testAddSize() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addSize(new SizeSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addFilename(FilenameSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addFilename(FilenameSelector)}
   */
  @Test
  public void testAddFilename() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addFilename(new FilenameSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addCustom(ExtendSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addCustom(ExtendSelector)}
   */
  @Test
  public void testAddCustom() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addCustom(new ExtendSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addContains(ContainsSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addContains(ContainsSelector)}
   */
  @Test
  public void testAddContains() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addContains(new ContainsSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addPresent(PresentSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addPresent(PresentSelector)}
   */
  @Test
  public void testAddPresent() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addPresent(new PresentSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addDepth(DepthSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addDepth(DepthSelector)}
   */
  @Test
  public void testAddDepth() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addDepth(new DepthSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addDepend(DependSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addDepend(DependSelector)}
   */
  @Test
  public void testAddDepend() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addDepend(new DependSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addContainsRegexp(ContainsRegexpSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addContainsRegexp(ContainsRegexpSelector)}
   */
  @Test
  public void testAddContainsRegexp() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addContainsRegexp(new ContainsRegexpSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addDifferent(DifferentSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addDifferent(DifferentSelector)}
   */
  @Test
  public void testAddDifferent() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addDifferent(new DifferentSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addType(TypeSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addType(TypeSelector)}
   */
  @Test
  public void testAddType() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addType(new TypeSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#addModified(ModifiedSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#addModified(ModifiedSelector)}
   */
  @Test
  public void testAddModified() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.addModified(new ModifiedSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#add(FileSelector)}.
   * <p>
   * Method under test: {@link MatchingTask#add(FileSelector)}
   */
  @Test
  public void testAdd() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act
    checksum.add(new ScriptSelector());

    // Assert
    assertTrue(checksum.hasSelectors());
  }

  /**
   * Test {@link MatchingTask#getImplicitFileSet()}.
   * <p>
   * Method under test: {@link MatchingTask#getImplicitFileSet()}
   */
  @Test
  public void testGetImplicitFileSet() {
    // Arrange
    Checksum checksum = new Checksum();

    // Act and Assert
    assertSame(checksum.fileset, checksum.getImplicitFileSet());
  }
}
