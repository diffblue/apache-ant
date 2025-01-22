package org.apache.tools.ant.filters.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FilterChain;
import org.junit.Test;

public class ChainReaderHelperDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ChainReaderHelper#ChainReaderHelper()}
   *   <li>{@link ChainReaderHelper#setBufferSize(int)}
   *   <li>{@link ChainReaderHelper#setFilterChains(Vector)}
   *   <li>{@link ChainReaderHelper#setPrimaryReader(Reader)}
   *   <li>{@link ChainReaderHelper#setProject(Project)}
   *   <li>{@link ChainReaderHelper#getProject()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws IOException {
    // Arrange and Act
    ChainReaderHelper actualChainReaderHelper = new ChainReaderHelper();
    actualChainReaderHelper.setBufferSize(3);
    actualChainReaderHelper.setFilterChains(new Vector<>());
    actualChainReaderHelper.setPrimaryReader(new StringReader("foo"));
    Project project = new Project();
    actualChainReaderHelper.setProject(project);
    Project actualProject = actualChainReaderHelper.getProject();

    // Assert
    assertTrue(actualChainReaderHelper.primaryReader.ready());
    assertSame(project, actualProject);
  }

  /**
   * Test {@link ChainReaderHelper#ChainReaderHelper(Project, Reader, Iterable)}.
   * <ul>
   *   <li>Then return Project BuildListeners is {@link ArrayList#ArrayList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainReaderHelper#ChainReaderHelper(Project, Reader, Iterable)}
   */
  @Test
  public void testNewChainReaderHelper_thenReturnProjectBuildListenersIsArrayList() {
    // Arrange
    Project project = new Project();
    StringReader primaryReader = new StringReader("foo");
    ArrayList<FilterChain> filterChains = new ArrayList<>();

    // Act
    ChainReaderHelper actualChainReaderHelper = new ChainReaderHelper(project, primaryReader, filterChains);

    // Assert
    assertTrue(project.getBuildListeners().isEmpty());
    assertEquals(filterChains, actualChainReaderHelper.getProject().getBuildListeners());
    assertEquals(filterChains, actualChainReaderHelper.filterChains);
  }

  /**
   * Test {@link ChainReaderHelper#ChainReaderHelper(Project, Reader, Iterable)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then return {@link ChainReaderHelper#filterChains} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainReaderHelper#ChainReaderHelper(Project, Reader, Iterable)}
   */
  @Test
  public void testNewChainReaderHelper_whenVector_thenReturnFilterChainsEmpty() {
    // Arrange
    Project project = new Project();
    StringReader primaryReader = new StringReader("foo");

    // Act
    ChainReaderHelper actualChainReaderHelper = new ChainReaderHelper(project, primaryReader, new Vector<>());

    // Assert
    assertTrue(project.getBuildListeners().isEmpty());
    assertTrue(actualChainReaderHelper.filterChains.isEmpty());
  }

  /**
   * Test {@link ChainReaderHelper#withPrimaryReader(Reader)}.
   * <p>
   * Method under test: {@link ChainReaderHelper#withPrimaryReader(Reader)}
   */
  @Test
  public void testWithPrimaryReader() throws IOException, BuildException {
    // Arrange
    ChainReaderHelper chainReaderHelper = new ChainReaderHelper();

    // Act
    ChainReaderHelper actualWithPrimaryReaderResult = chainReaderHelper.withPrimaryReader(new StringReader("foo"));

    // Assert
    assertTrue(chainReaderHelper.getAssembledReader().ready());
    assertTrue(chainReaderHelper.primaryReader.ready());
    assertSame(chainReaderHelper, actualWithPrimaryReaderResult);
  }

  /**
   * Test {@link ChainReaderHelper#withProject(Project)}.
   * <p>
   * Method under test: {@link ChainReaderHelper#withProject(Project)}
   */
  @Test
  public void testWithProject() {
    // Arrange
    ChainReaderHelper chainReaderHelper = new ChainReaderHelper();
    Project project = new Project();

    // Act
    ChainReaderHelper actualWithProjectResult = chainReaderHelper.withProject(project);

    // Assert
    assertTrue(project.getBuildListeners().isEmpty());
    assertSame(project, chainReaderHelper.getProject());
    assertSame(chainReaderHelper, actualWithProjectResult);
  }

  /**
   * Test {@link ChainReaderHelper#withFilterChains(Iterable)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainReaderHelper#withFilterChains(Iterable)}
   */
  @Test
  public void testWithFilterChains_whenArrayList_thenArrayListEmpty() {
    // Arrange
    ChainReaderHelper chainReaderHelper = new ChainReaderHelper();
    ArrayList<FilterChain> filterChains = new ArrayList<>();

    // Act
    chainReaderHelper.withFilterChains(filterChains);

    // Assert
    assertTrue(filterChains.isEmpty());
  }

  /**
   * Test {@link ChainReaderHelper#withFilterChains(Iterable)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link Vector#Vector()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainReaderHelper#withFilterChains(Iterable)}
   */
  @Test
  public void testWithFilterChains_whenVector_thenVectorEmpty() {
    // Arrange
    ChainReaderHelper chainReaderHelper = new ChainReaderHelper();
    Vector<FilterChain> filterChains = new Vector<>();

    // Act
    ChainReaderHelper actualWithFilterChainsResult = chainReaderHelper.withFilterChains(filterChains);

    // Assert
    assertTrue(filterChains.isEmpty());
    assertSame(chainReaderHelper, actualWithFilterChainsResult);
  }

  /**
   * Test {@link ChainReaderHelper#getAssembledReader()}.
   * <ul>
   *   <li>Given {@link ChainReaderHelper#ChainReaderHelper()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainReaderHelper#getAssembledReader()}
   */
  @Test
  public void testGetAssembledReader_givenChainReaderHelper_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ChainReaderHelper()).getAssembledReader());
  }

  /**
   * Test {@link ChainReaderHelper#readFully(Reader)}.
   * <ul>
   *   <li>Given {@link ChainReaderHelper#ChainReaderHelper()}.</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainReaderHelper#readFully(Reader)}
   */
  @Test
  public void testReadFully_givenChainReaderHelper_whenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange
    ChainReaderHelper chainReaderHelper = new ChainReaderHelper();

    // Act and Assert
    assertEquals("foo", chainReaderHelper.readFully(new StringReader("foo")));
  }

  /**
   * Test {@link ChainReaderHelper#readFully(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainReaderHelper#readFully(Reader)}
   */
  @Test
  public void testReadFully_whenStringReaderWithEmptyString_thenReturnNull() throws IOException {
    // Arrange
    ChainReaderHelper chainReaderHelper = new ChainReaderHelper();

    // Act and Assert
    assertNull(chainReaderHelper.readFully(new StringReader("")));
  }
}
