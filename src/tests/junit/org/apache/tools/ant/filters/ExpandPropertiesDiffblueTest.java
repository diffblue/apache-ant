package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.PropertySet;
import org.junit.Test;

public class ExpandPropertiesDiffblueTest {
  /**
   * Test {@link ExpandProperties#ExpandProperties(Reader)}.
   * <p>
   * Method under test: {@link ExpandProperties#ExpandProperties(Reader)}
   */
  @Test
  public void testNewExpandProperties() {
    // Arrange and Act
    ExpandProperties actualExpandProperties = new ExpandProperties(new StringReader("foo"));

    // Assert
    assertNull(actualExpandProperties.getProject());
    assertFalse(actualExpandProperties.getInitialized());
  }

  /**
   * Test {@link ExpandProperties#ExpandProperties()}.
   * <p>
   * Method under test: {@link ExpandProperties#ExpandProperties()}
   */
  @Test
  public void testNewExpandProperties2() {
    // Arrange and Act
    ExpandProperties actualExpandProperties = new ExpandProperties();

    // Assert
    assertNull(actualExpandProperties.getProject());
    assertFalse(actualExpandProperties.getInitialized());
  }

  /**
   * Test {@link ExpandProperties#add(PropertySet)}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()} add {@link PropertySet} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#add(PropertySet)}
   */
  @Test
  public void testAdd_givenExpandPropertiesAddPropertySet_thenThrowBuildException() {
    // Arrange
    ExpandProperties expandProperties = new ExpandProperties();
    expandProperties.add(new PropertySet());

    // Act and Assert
    assertThrows(BuildException.class, () -> expandProperties.add(new PropertySet()));
  }

  /**
   * Test {@link ExpandProperties#read()}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties(Reader)} with in is {@link StringReader#StringReader(String)} add {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#read()}
   */
  @Test
  public void testRead_givenExpandPropertiesWithInIsStringReaderAddPropertySet() throws IOException {
    // Arrange
    ExpandProperties expandProperties = new ExpandProperties(new StringReader("foo"));
    expandProperties.add(new PropertySet());

    // Act and Assert
    assertEquals(102, expandProperties.read());
  }

  /**
   * Test {@link ExpandProperties#read()}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties(Reader)} with in is {@link StringReader#StringReader(String)} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#read()}
   */
  @Test
  public void testRead_givenExpandPropertiesWithInIsStringReaderProjectIsProject() throws IOException {
    // Arrange
    ExpandProperties expandProperties = new ExpandProperties(new StringReader("foo"));
    expandProperties.setProject(new Project());
    expandProperties.add(new PropertySet());

    // Act and Assert
    assertEquals(102, expandProperties.read());
  }

  /**
   * Test {@link ExpandProperties#read()}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties(Reader)} with in is {@link StringReader#StringReader(String)} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#read()}
   */
  @Test
  public void testRead_givenExpandPropertiesWithInIsStringReaderProjectIsProject2() throws IOException {
    // Arrange
    ExpandProperties expandProperties = new ExpandProperties(new StringReader("foo"));
    expandProperties.setProject(new Project());

    // Act and Assert
    assertEquals(102, expandProperties.read());
  }

  /**
   * Test {@link ExpandProperties#read()}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#read()}
   */
  @Test
  public void testRead_givenExpandPropertiesWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange, Act and Assert
    assertEquals(102, (new ExpandProperties(new StringReader("foo"))).read());
  }

  /**
   * Test {@link ExpandProperties#read()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#read()}
   */
  @Test
  public void testRead_givenProjectAddBuildListenerAntClassLoader_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ExpandProperties expandProperties = new ExpandProperties(new StringReader("foo"));
    expandProperties.setProject(project);
    expandProperties.add(new PropertySet());

    // Act and Assert
    assertEquals(102, expandProperties.read());
  }

  /**
   * Test {@link ExpandProperties#read()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#read()}
   */
  @Test
  public void testRead_givenProjectAddTargetAddingReferenceAndTarget_thenReturnOneHundredTwo()
      throws IOException, BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    ExpandProperties expandProperties = new ExpandProperties(new StringReader("foo"));
    expandProperties.setProject(project);
    expandProperties.add(new PropertySet());

    // Act and Assert
    assertEquals(102, expandProperties.read());
  }

  /**
   * Test {@link ExpandProperties#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new ExpandProperties(new StringReader(""))).read());
  }

  /**
   * Test {@link ExpandProperties#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link ExpandProperties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExpandProperties#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnExpandProperties() throws IOException {
    // Arrange
    ExpandProperties expandProperties = new ExpandProperties();

    // Act
    Reader actualChainResult = expandProperties.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof ExpandProperties);
    assertEquals("foo", ((ExpandProperties) actualChainResult).readFully());
    assertNull(((ExpandProperties) actualChainResult).getProject());
    assertFalse(((ExpandProperties) actualChainResult).getInitialized());
    assertTrue(actualChainResult.ready());
  }
}
