package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.PatternSet;
import org.apache.tools.ant.types.PatternSet.NameEntry;
import org.apache.tools.ant.types.PatternSet.PatternFileNameEntry;
import org.apache.tools.ant.types.TarFileSet;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.optional.depend.ClassfileSet;
import org.apache.tools.ant.types.selectors.AndSelector;
import org.apache.tools.ant.types.selectors.ContainsRegexpSelector;
import org.apache.tools.ant.types.selectors.ContainsSelector;
import org.apache.tools.ant.types.selectors.DateSelector;
import org.apache.tools.ant.types.selectors.DependSelector;
import org.apache.tools.ant.types.selectors.DepthSelector;
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
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector;
import org.junit.Test;

public class DeleteDiffblueTest {
  /**
   * Test {@link Delete#setDir(File)}.
   * <p>
   * Method under test: {@link Delete#setDir(File)}
   */
  @Test
  public void testSetDir() {
    // Arrange
    Delete delete = new Delete();
    File dir = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    delete.setDir(dir);

    // Assert
    File file = delete.dir;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
    assertSame(dir, delete.getImplicitFileSet().getDir());
  }

  /**
   * Test {@link Delete#addFileset(FileSet)}.
   * <p>
   * Method under test: {@link Delete#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset() {
    // Arrange
    Delete delete = new Delete();
    FileSet set = new FileSet();

    // Act
    delete.addFileset(set);

    // Assert
    Vector<FileSet> fileSetList = delete.filesets;
    assertEquals(1, fileSetList.size());
    assertSame(set, fileSetList.get(0));
  }

  /**
   * Test {@link Delete#add(FileSelector)} with {@code selector}.
   * <p>
   * Method under test: {@link Delete#add(FileSelector)}
   */
  @Test
  public void testAddWithSelector() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.add(new ScriptSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#execute()}.
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    delete.addSelector(new SelectSelector());
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) addFileset {@link ClassfileSet#ClassfileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteAddFilesetClassfileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.addFileset(new ClassfileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) addFileset {@link TarFileSet#TarFileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteAddFilesetTarFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.addFileset(new TarFileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) addSelector {@link SelectSelector} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteAddSelectorSelectSelector_thenThrowBuildException() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.addSelector(new SelectSelector());
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) Dir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteDirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.setDir(Copy.NULL_FILE_PLACEHOLDER);
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) Dir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteDirIsNull_file_placeholder_thenThrowBuildException2() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.setDir(Copy.NULL_FILE_PLACEHOLDER);
    delete.addSelector(new SelectSelector());
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) File is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteFileIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.setFile(Copy.NULL_FILE_PLACEHOLDER);
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.setProject(new Project());
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDeleteProjectIsProject_thenThrowBuildException2() throws BuildException {
    // Arrange
    Delete delete = new Delete();
    delete.setProject(new Project());
    delete.addSelector(new SelectSelector());
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenDelete_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Delete()).execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link FileSet#FileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenFileSetAppendSelectorScriptSelector_thenThrowBuildException() throws BuildException {
    // Arrange
    FileSet set = new FileSet();
    set.appendSelector(new ScriptSelector());

    Delete delete = new Delete();
    delete.addFileset(set);

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link FileSet#FileSet()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenFileSetProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    FileSet set = new FileSet();
    set.setProject(new Project());
    set.appendSelector(new ScriptSelector());

    Delete delete = new Delete();
    delete.addFileset(set);

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Delete delete = new Delete();
    delete.setProject(project);
    delete.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> delete.execute());
  }

  /**
   * Test {@link Delete#removeDir(File)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#removeDir(File)}
   */
  @Test
  public void testRemoveDir_thenThrowBuildException() {
    // Arrange
    Delete delete = new Delete();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> delete.removeDir(Paths.get(System.getProperty("java.io.tmpdir"), "Deleting directory ").toFile()));
  }

  /**
   * Test {@link Delete#removeFiles(File, String[], String[])}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) IncludeEmptyDirs is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#removeFiles(File, String[], String[])}
   */
  @Test
  public void testRemoveFiles_givenDeleteIncludeEmptyDirsIsTrue_thenThrowBuildException() {
    // Arrange
    Delete delete = new Delete();
    delete.setIncludeEmptyDirs(true);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> delete.removeFiles(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), new String[]{},
            new String[]{"Dirs"}));
  }

  /**
   * Test {@link Delete#removeFiles(File, String[], String[])}.
   * <ul>
   *   <li>Given {@link Delete} (default constructor) PerformGcOnFailedDelete is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#removeFiles(File, String[], String[])}
   */
  @Test
  public void testRemoveFiles_givenDeletePerformGcOnFailedDeleteIsTrue_thenThrowBuildException() {
    // Arrange
    Delete delete = new Delete();
    delete.setPerformGcOnFailedDelete(true);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> delete.removeFiles(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
            new String[]{"Files"}, new String[]{"Dirs"}));
  }

  /**
   * Test {@link Delete#removeFiles(File, String[], String[])}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#removeFiles(File, String[], String[])}
   */
  @Test
  public void testRemoveFiles_thenThrowBuildException() {
    // Arrange
    Delete delete = new Delete();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> delete.removeFiles(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
            new String[]{"Files"}, new String[]{"Dirs"}));
  }

  /**
   * Test {@link Delete#createInclude()}.
   * <p>
   * Method under test: {@link Delete#createInclude()}
   */
  @Test
  public void testCreateInclude() {
    // Arrange
    Delete delete = new Delete();

    // Act and Assert
    assertNull(delete.createInclude().getName());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#createIncludesFile()}.
   * <p>
   * Method under test: {@link Delete#createIncludesFile()}
   */
  @Test
  public void testCreateIncludesFile() {
    // Arrange
    Delete delete = new Delete();

    // Act
    NameEntry actualCreateIncludesFileResult = delete.createIncludesFile();

    // Assert
    assertTrue(actualCreateIncludesFileResult instanceof PatternFileNameEntry);
    assertNull(actualCreateIncludesFileResult.getName());
    assertNull(((PatternFileNameEntry) actualCreateIncludesFileResult).getEncoding());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#createExclude()}.
   * <p>
   * Method under test: {@link Delete#createExclude()}
   */
  @Test
  public void testCreateExclude() {
    // Arrange
    Delete delete = new Delete();

    // Act and Assert
    assertNull(delete.createExclude().getName());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#createExcludesFile()}.
   * <p>
   * Method under test: {@link Delete#createExcludesFile()}
   */
  @Test
  public void testCreateExcludesFile() {
    // Arrange
    Delete delete = new Delete();

    // Act
    NameEntry actualCreateExcludesFileResult = delete.createExcludesFile();

    // Assert
    assertTrue(actualCreateExcludesFileResult instanceof PatternFileNameEntry);
    assertNull(actualCreateExcludesFileResult.getName());
    assertNull(((PatternFileNameEntry) actualCreateExcludesFileResult).getEncoding());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#createPatternSet()}.
   * <p>
   * Method under test: {@link Delete#createPatternSet()}
   */
  @Test
  public void testCreatePatternSet() {
    // Arrange
    Delete delete = new Delete();

    // Act
    PatternSet actualCreatePatternSetResult = delete.createPatternSet();

    // Assert
    Location location = actualCreatePatternSetResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreatePatternSetResult.getDescription());
    assertNull(actualCreatePatternSetResult.getProject());
    assertNull(actualCreatePatternSetResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreatePatternSetResult.isReference());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#setIncludes(String)}.
   * <p>
   * Method under test: {@link Delete#setIncludes(String)}
   */
  @Test
  public void testSetIncludes() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.setIncludes("Includes");

    // Assert
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#setExcludes(String)}.
   * <p>
   * Method under test: {@link Delete#setExcludes(String)}
   */
  @Test
  public void testSetExcludes() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.setExcludes("Excludes");

    // Assert
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#setDefaultexcludes(boolean)}.
   * <p>
   * Method under test: {@link Delete#setDefaultexcludes(boolean)}
   */
  @Test
  public void testSetDefaultexcludes() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.setDefaultexcludes(true);

    // Assert
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#setIncludesfile(File)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then {@link Delete} (default constructor) {@link Delete#usedMatchingTask}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#setIncludesfile(File)}
   */
  @Test
  public void testSetIncludesfile_whenNull_file_placeholder_thenDeleteUsedMatchingTask() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.setIncludesfile(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#setExcludesfile(File)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then {@link Delete} (default constructor) {@link Delete#usedMatchingTask}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delete#setExcludesfile(File)}
   */
  @Test
  public void testSetExcludesfile_whenNull_file_placeholder_thenDeleteUsedMatchingTask() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.setExcludesfile(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#setCaseSensitive(boolean)}.
   * <p>
   * Method under test: {@link Delete#setCaseSensitive(boolean)}
   */
  @Test
  public void testSetCaseSensitive() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.setCaseSensitive(true);

    // Assert
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#setFollowSymlinks(boolean)}.
   * <p>
   * Method under test: {@link Delete#setFollowSymlinks(boolean)}
   */
  @Test
  public void testSetFollowSymlinks() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.setFollowSymlinks(true);

    // Assert
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addSelector(SelectSelector)}.
   * <p>
   * Method under test: {@link Delete#addSelector(SelectSelector)}
   */
  @Test
  public void testAddSelector() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addSelector(new SelectSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addAnd(AndSelector)}.
   * <p>
   * Method under test: {@link Delete#addAnd(AndSelector)}
   */
  @Test
  public void testAddAnd() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addAnd(new AndSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addOr(OrSelector)}.
   * <p>
   * Method under test: {@link Delete#addOr(OrSelector)}
   */
  @Test
  public void testAddOr() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addOr(new OrSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addNot(NotSelector)}.
   * <p>
   * Method under test: {@link Delete#addNot(NotSelector)}
   */
  @Test
  public void testAddNot() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addNot(new NotSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addNone(NoneSelector)}.
   * <p>
   * Method under test: {@link Delete#addNone(NoneSelector)}
   */
  @Test
  public void testAddNone() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addNone(new NoneSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addMajority(MajoritySelector)}.
   * <p>
   * Method under test: {@link Delete#addMajority(MajoritySelector)}
   */
  @Test
  public void testAddMajority() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addMajority(new MajoritySelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addDate(DateSelector)}.
   * <p>
   * Method under test: {@link Delete#addDate(DateSelector)}
   */
  @Test
  public void testAddDate() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addDate(new DateSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addSize(SizeSelector)}.
   * <p>
   * Method under test: {@link Delete#addSize(SizeSelector)}
   */
  @Test
  public void testAddSize() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addSize(new SizeSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addFilename(FilenameSelector)}.
   * <p>
   * Method under test: {@link Delete#addFilename(FilenameSelector)}
   */
  @Test
  public void testAddFilename() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addFilename(new FilenameSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addCustom(ExtendSelector)}.
   * <p>
   * Method under test: {@link Delete#addCustom(ExtendSelector)}
   */
  @Test
  public void testAddCustom() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addCustom(new ExtendSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addContains(ContainsSelector)}.
   * <p>
   * Method under test: {@link Delete#addContains(ContainsSelector)}
   */
  @Test
  public void testAddContains() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addContains(new ContainsSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addPresent(PresentSelector)}.
   * <p>
   * Method under test: {@link Delete#addPresent(PresentSelector)}
   */
  @Test
  public void testAddPresent() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addPresent(new PresentSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addDepth(DepthSelector)}.
   * <p>
   * Method under test: {@link Delete#addDepth(DepthSelector)}
   */
  @Test
  public void testAddDepth() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addDepth(new DepthSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addDepend(DependSelector)}.
   * <p>
   * Method under test: {@link Delete#addDepend(DependSelector)}
   */
  @Test
  public void testAddDepend() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addDepend(new DependSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addContainsRegexp(ContainsRegexpSelector)}.
   * <p>
   * Method under test: {@link Delete#addContainsRegexp(ContainsRegexpSelector)}
   */
  @Test
  public void testAddContainsRegexp() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addContainsRegexp(new ContainsRegexpSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test {@link Delete#addModified(ModifiedSelector)}.
   * <p>
   * Method under test: {@link Delete#addModified(ModifiedSelector)}
   */
  @Test
  public void testAddModified() {
    // Arrange
    Delete delete = new Delete();

    // Act
    delete.addModified(new ModifiedSelector());

    // Assert
    assertTrue(delete.hasSelectors());
    assertTrue(delete.usedMatchingTask);
  }

  /**
   * Test new {@link Delete} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Delete}
   */
  @Test
  public void testNewDelete() {
    // Arrange and Act
    Delete actualDelete = new Delete();

    // Assert
    assertNull(actualDelete.dir);
    assertNull(actualDelete.file);
    assertNull(actualDelete.getDescription());
    assertNull(actualDelete.getTaskName());
    assertNull(actualDelete.getTaskType());
    assertNull(actualDelete.getProject());
    assertNull(actualDelete.getOwningTarget());
    assertFalse(actualDelete.hasSelectors());
    assertFalse(actualDelete.includeEmpty);
    assertFalse(actualDelete.usedMatchingTask);
    assertTrue(actualDelete.filesets.isEmpty());
  }
}
