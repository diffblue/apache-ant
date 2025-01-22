package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Iterator;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.filters.FixCrLfFilter;
import org.apache.tools.ant.taskdefs.FixCRLF.AddAsisRemove;
import org.apache.tools.ant.taskdefs.FixCRLF.CrLf;
import org.apache.tools.ant.taskdefs.FixCRLF.OneLiner;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.resources.FileResourceIterator;
import org.junit.Test;

public class FixCRLFDiffblueTest {
  /**
   * Test AddAsisRemove {@link AddAsisRemove#getValues()}.
   * <p>
   * Method under test: {@link AddAsisRemove#getValues()}
   */
  @Test
  public void testAddAsisRemoveGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"add", "asis", "remove"}, (new AddAsisRemove()).getValues());
  }

  /**
   * Test AddAsisRemove new {@link AddAsisRemove} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AddAsisRemove}
   */
  @Test
  public void testAddAsisRemoveNewAddAsisRemove() {
    // Arrange and Act
    AddAsisRemove actualAddAsisRemove = new AddAsisRemove();

    // Assert
    assertNull(actualAddAsisRemove.getValue());
    assertEquals(-1, actualAddAsisRemove.getIndex());
  }

  /**
   * Test {@link FixCRLF#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link FixCrLfFilter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnFixCrLfFilter() throws IOException {
    // Arrange
    FixCRLF fixCRLF = new FixCRLF();

    // Act
    Reader actualChainResult = fixCRLF.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof FixCrLfFilter);
    FixCrLfFilter.AddAsisRemove tab = ((FixCrLfFilter) actualChainResult).getTab();
    assertEquals("asis", tab.getValue());
    FixCrLfFilter.CrLf eol = ((FixCrLfFilter) actualChainResult).getEol();
    assertEquals("lf", eol.getValue());
    FixCrLfFilter.AddAsisRemove eof = ((FixCrLfFilter) actualChainResult).getEof();
    assertEquals("remove", eof.getValue());
    assertEquals(1, tab.getIndex());
    assertEquals(2, eof.getIndex());
    assertEquals(2, eol.getIndex());
    assertEquals(3, eof.getValues().length);
    assertEquals(3, tab.getValues().length);
    assertEquals(7, eol.getValues().length);
    assertEquals(8, ((FixCrLfFilter) actualChainResult).getTablength());
    assertFalse(((FixCrLfFilter) actualChainResult).getJavafiles());
    assertTrue(actualChainResult.ready());
    assertTrue(((FixCrLfFilter) actualChainResult).getFixlast());
  }

  /**
   * Test CrLf {@link CrLf#getValues()}.
   * <p>
   * Method under test: {@link CrLf#getValues()}
   */
  @Test
  public void testCrLfGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"asis", "cr", "lf", "crlf", "mac", "unix", "dos"}, (new CrLf()).getValues());
  }

  /**
   * Test CrLf new {@link CrLf} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CrLf}
   */
  @Test
  public void testCrLfNewCrLf() {
    // Arrange and Act
    CrLf actualCrLf = new CrLf();

    // Assert
    assertNull(actualCrLf.getValue());
    assertEquals(-1, actualCrLf.getIndex());
  }

  /**
   * Test OneLiner {@link OneLiner#OneLiner(FixCRLF, File)}.
   * <ul>
   *   <li>Given {@code this$0}.</li>
   *   <li>When {@link FixCRLF} (default constructor) Encoding is {@code this$0}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OneLiner#OneLiner(FixCRLF, File)}
   */
  @Test
  public void testOneLinerNewOneLiner_givenThis0_whenFixCRLFEncodingIsThis0() throws BuildException {
    // Arrange
    FixCRLF fixCRLF = new FixCRLF();
    fixCRLF.setEncoding("this$0");

    // Act and Assert
    assertThrows(BuildException.class, () -> fixCRLF.new OneLiner(Copy.NULL_FILE_PLACEHOLDER));

  }

  /**
   * Test OneLiner {@link OneLiner#OneLiner(FixCRLF, File)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link FixCRLF} (default constructor) Javafiles is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OneLiner#OneLiner(FixCRLF, File)}
   */
  @Test
  public void testOneLinerNewOneLiner_givenTrue_whenFixCRLFJavafilesIsTrue() throws BuildException {
    // Arrange
    FixCRLF fixCRLF = new FixCRLF();
    fixCRLF.setJavafiles(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> fixCRLF.new OneLiner(Copy.NULL_FILE_PLACEHOLDER));

  }

  /**
   * Test OneLiner {@link OneLiner#OneLiner(FixCRLF, File)}.
   * <ul>
   *   <li>When {@link FixCRLF} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OneLiner#OneLiner(FixCRLF, File)}
   */
  @Test
  public void testOneLinerNewOneLiner_whenFixCRLF_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FixCRLF()).new OneLiner(Copy.NULL_FILE_PLACEHOLDER));

  }

  /**
   * Test {@link FixCRLF#setTablength(int)}.
   * <ul>
   *   <li>When eighty-one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#setTablength(int)}
   */
  @Test
  public void testSetTablength_whenEightyOne_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FixCRLF()).setTablength(81));
  }

  /**
   * Test {@link FixCRLF#setTablength(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#setTablength(int)}
   */
  @Test
  public void testSetTablength_whenOne_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FixCRLF()).setTablength(1));
  }

  /**
   * Test {@link FixCRLF#execute()}.
   * <ul>
   *   <li>Given {@link FixCRLF} (default constructor) Destdir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#execute()}
   */
  @Test
  public void testExecute_givenFixCRLFDestdirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    FixCRLF fixCRLF = new FixCRLF();
    fixCRLF.setDestdir(Copy.NULL_FILE_PLACEHOLDER);
    fixCRLF.appendSelector(new ScriptSelector());
    fixCRLF.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> fixCRLF.execute());
  }

  /**
   * Test {@link FixCRLF#execute()}.
   * <ul>
   *   <li>Given {@link FixCRLF} (default constructor) Srcdir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#execute()}
   */
  @Test
  public void testExecute_givenFixCRLFSrcdirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    FixCRLF fixCRLF = new FixCRLF();
    fixCRLF.setSrcdir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> fixCRLF.execute());
  }

  /**
   * Test {@link FixCRLF#execute()}.
   * <ul>
   *   <li>Given {@link FixCRLF} (default constructor) Srcdir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#execute()}
   */
  @Test
  public void testExecute_givenFixCRLFSrcdirIsNull_file_placeholder_thenThrowBuildException2() throws BuildException {
    // Arrange
    FixCRLF fixCRLF = new FixCRLF();
    fixCRLF.setSrcdir(Copy.NULL_FILE_PLACEHOLDER);
    fixCRLF.appendSelector(new ScriptSelector());
    fixCRLF.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> fixCRLF.execute());
  }

  /**
   * Test {@link FixCRLF#execute()}.
   * <ul>
   *   <li>Given {@link FixCRLF} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#execute()}
   */
  @Test
  public void testExecute_givenFixCRLF_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FixCRLF()).execute());
  }

  /**
   * Test {@link FixCRLF#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    FixCRLF fixCRLF = new FixCRLF();
    fixCRLF.setProject(project);
    fixCRLF.appendSelector(new ScriptSelector());
    fixCRLF.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    fixCRLF.execute();

    // Assert
    FileSet implicitFileSet = fixCRLF.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    File dir = implicitFileSet.getDir();
    assertEquals("", dir.getName());
    DirectoryScanner directoryScanner = implicitFileSet.getDirectoryScanner();
    assertEquals(0, directoryScanner.getIncludedDirsCount());
    assertEquals(0, directoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, directoryScanner.getIncludedDirectories().length);
    assertEquals(0, directoryScanner.getIncludedFiles().length);
    assertEquals(0, directoryScanner.getNotFollowedSymlinks().length);
    assertFalse(iteratorResult.hasNext());
    assertTrue(dir.isAbsolute());
    assertTrue(directoryScanner.isCaseSensitive());
    assertTrue(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
    assertSame(dir, directoryScanner.getBasedir());
  }

  /**
   * Test {@link FixCRLF#execute()}.
   * <ul>
   *   <li>Then {@link FixCRLF} (default constructor) ImplicitFileSet iterator {@link FileResourceIterator}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCRLF#execute()}
   */
  @Test
  public void testExecute_thenFixCRLFImplicitFileSetIteratorFileResourceIterator() throws BuildException {
    // Arrange
    FixCRLF fixCRLF = new FixCRLF();
    fixCRLF.setProject(new Project());
    fixCRLF.appendSelector(new ScriptSelector());
    fixCRLF.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    fixCRLF.execute();

    // Assert
    FileSet implicitFileSet = fixCRLF.getImplicitFileSet();
    Iterator<Resource> iteratorResult = implicitFileSet.iterator();
    assertTrue(iteratorResult instanceof FileResourceIterator);
    File dir = implicitFileSet.getDir();
    assertEquals("", dir.getName());
    DirectoryScanner directoryScanner = implicitFileSet.getDirectoryScanner();
    assertEquals(0, directoryScanner.getIncludedDirsCount());
    assertEquals(0, directoryScanner.getIncludedFilesCount());
    assertEquals(0, implicitFileSet.size());
    assertEquals(0, directoryScanner.getIncludedDirectories().length);
    assertEquals(0, directoryScanner.getIncludedFiles().length);
    assertEquals(0, directoryScanner.getNotFollowedSymlinks().length);
    assertFalse(iteratorResult.hasNext());
    assertTrue(dir.isAbsolute());
    assertTrue(directoryScanner.isCaseSensitive());
    assertTrue(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.isFollowSymlinks());
    assertTrue(implicitFileSet.isEmpty());
    assertSame(dir, directoryScanner.getBasedir());
  }

  /**
   * Test new {@link FixCRLF} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FixCRLF}
   */
  @Test
  public void testNewFixCRLF() {
    // Arrange and Act
    FixCRLF actualFixCRLF = new FixCRLF();

    // Assert
    assertNull(actualFixCRLF.getDescription());
    assertNull(actualFixCRLF.getTaskName());
    assertNull(actualFixCRLF.getTaskType());
    assertNull(actualFixCRLF.getProject());
    assertNull(actualFixCRLF.getOwningTarget());
    assertFalse(actualFixCRLF.hasSelectors());
  }
}
