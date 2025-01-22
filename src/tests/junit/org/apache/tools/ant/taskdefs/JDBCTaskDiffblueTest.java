package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.sql.Connection;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class JDBCTaskDiffblueTest {
  /**
   * Test {@link JDBCTask#createClasspath()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenSQLExecProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    Project project = new Project();
    sqlExec.setProject(project);

    // Act and Assert
    assertSame(project, sqlExec.createClasspath().getProject());
  }

  /**
   * Test {@link JDBCTask#createClasspath()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor).</li>
   *   <li>Then {@link SQLExec} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenSQLExec_thenSQLExecClasspathDescriptionIsNull() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    Path actualCreateClasspathResult = sqlExec.createClasspath();

    // Assert
    Path classpath = sqlExec.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link JDBCTask#createClasspath()}.
   * <ul>
   *   <li>Then {@link SQLExec} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenSQLExecClasspathIsSystemBootClasspath() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = sqlExec.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, sqlExec.getClasspath());
  }

  /**
   * Test {@link JDBCTask#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenSQLExecClasspathIsSystemBootClasspath() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setClasspath(Path.systemBootClasspath);

    // Act
    sqlExec.setClasspathRef(new Reference("42"));

    // Assert that nothing has changed
    assertFalse(sqlExec.getClasspath().isReference());
  }

  /**
   * Test {@link JDBCTask#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor).</li>
   *   <li>Then {@link SQLExec} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenSQLExec_thenSQLExecClasspathDescriptionIsNull() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    sqlExec.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = sqlExec.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link JDBCTask#setUrl(String)}.
   * <p>
   * Method under test: {@link JDBCTask#setUrl(String)}
   */
  @Test
  public void testSetUrl() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    sqlExec.setUrl("https://example.org/example");

    // Assert
    assertEquals("https://example.org/example", sqlExec.getUrl());
  }

  /**
   * Test {@link JDBCTask#setPassword(String)}.
   * <p>
   * Method under test: {@link JDBCTask#setPassword(String)}
   */
  @Test
  public void testSetPassword() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    sqlExec.setPassword("iloveyou");

    // Assert
    assertEquals("iloveyou", sqlExec.getPassword());
  }

  /**
   * Test {@link JDBCTask#setAutocommit(boolean)}.
   * <p>
   * Method under test: {@link JDBCTask#setAutocommit(boolean)}
   */
  @Test
  public void testSetAutocommit() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    sqlExec.setAutocommit(true);

    // Assert
    assertTrue(sqlExec.isAutocommit());
  }

  /**
   * Test {@link JDBCTask#setRdbms(String)}.
   * <p>
   * Method under test: {@link JDBCTask#setRdbms(String)}
   */
  @Test
  public void testSetRdbms() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    sqlExec.setRdbms("Rdbms");

    // Assert
    assertEquals("Rdbms", sqlExec.getRdbms());
  }

  /**
   * Test {@link JDBCTask#setVersion(String)}.
   * <p>
   * Method under test: {@link JDBCTask#setVersion(String)}
   */
  @Test
  public void testSetVersion() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    sqlExec.setVersion("1.0.2");

    // Assert
    assertEquals("1.0.2", sqlExec.getVersion());
  }

  /**
   * Test {@link JDBCTask#isValidRdbms(Connection)}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#isValidRdbms(Connection)}
   */
  @Test
  public void testIsValidRdbms_givenSQLExec_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new SQLExec()).isValidRdbms(null));
  }

  /**
   * Test {@link JDBCTask#getLoader()}.
   * <p>
   * Method under test: {@link JDBCTask#getLoader()}
   */
  @Test
  public void testGetLoader() {
    // Arrange, Act and Assert
    assertNull((new SQLExec()).getLoader());
  }

  /**
   * Test {@link JDBCTask#getConnection()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) addConnectionProperty {@link Property#Property()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#getConnection()}
   */
  @Test
  public void testGetConnection_givenSQLExecAddConnectionPropertyProperty() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.addConnectionProperty(new Property());
    sqlExec.setUrl("https://example.org/example");
    sqlExec.setPassword("iloveyou");
    sqlExec.setUserid("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> sqlExec.getConnection());
  }

  /**
   * Test {@link JDBCTask#getConnection()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Driver is {@code connecting to}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#getConnection()}
   */
  @Test
  public void testGetConnection_givenSQLExecDriverIsConnectingTo_thenThrowBuildException() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setDriver("connecting to ");
    sqlExec.addConnectionProperty(new Property());
    sqlExec.setUrl("https://example.org/example");
    sqlExec.setPassword("iloveyou");
    sqlExec.setUserid("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> sqlExec.getConnection());
  }

  /**
   * Test {@link JDBCTask#getConnection()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Driver is {@code java.sql.Driver}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#getConnection()}
   */
  @Test
  public void testGetConnection_givenSQLExecDriverIsJavaSqlDriver_thenThrowBuildException() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setDriver("java.sql.Driver");
    sqlExec.addConnectionProperty(new Property());
    sqlExec.setUrl("https://example.org/example");
    sqlExec.setPassword("iloveyou");
    sqlExec.setUserid("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> sqlExec.getConnection());
  }

  /**
   * Test {@link JDBCTask#getConnection()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Password is {@code iloveyou}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#getConnection()}
   */
  @Test
  public void testGetConnection_givenSQLExecPasswordIsIloveyou_thenThrowBuildException() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setPassword("iloveyou");
    sqlExec.setUserid("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> sqlExec.getConnection());
  }

  /**
   * Test {@link JDBCTask#getConnection()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#getConnection()}
   */
  @Test
  public void testGetConnection_givenSQLExecProjectIsProject_thenThrowBuildException() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setProject(new Project());
    sqlExec.addConnectionProperty(new Property());
    sqlExec.setUrl("https://example.org/example");
    sqlExec.setPassword("iloveyou");
    sqlExec.setUserid("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> sqlExec.getConnection());
  }

  /**
   * Test {@link JDBCTask#getConnection()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Url is {@code https://example.org/example}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#getConnection()}
   */
  @Test
  public void testGetConnection_givenSQLExecUrlIsHttpsExampleOrgExample() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setUrl("https://example.org/example");
    sqlExec.setPassword("iloveyou");
    sqlExec.setUserid("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> sqlExec.getConnection());
  }

  /**
   * Test {@link JDBCTask#getConnection()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Userid is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#getConnection()}
   */
  @Test
  public void testGetConnection_givenSQLExecUseridIs42_thenThrowBuildException() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setUserid("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> sqlExec.getConnection());
  }

  /**
   * Test {@link JDBCTask#getConnection()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#getConnection()}
   */
  @Test
  public void testGetConnection_givenSQLExec_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SQLExec()).getConnection());
  }

  /**
   * Test {@link JDBCTask#getClasspath()}.
   * <p>
   * Method under test: {@link JDBCTask#getClasspath()}
   */
  @Test
  public void testGetClasspath() {
    // Arrange, Act and Assert
    assertNull((new SQLExec()).getClasspath());
  }

  /**
   * Test {@link JDBCTask#isAutocommit()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Autocommit is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#isAutocommit()}
   */
  @Test
  public void testIsAutocommit_givenSQLExecAutocommitIsTrue_thenReturnTrue() {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setAutocommit(true);

    // Act and Assert
    assertTrue(sqlExec.isAutocommit());
  }

  /**
   * Test {@link JDBCTask#isAutocommit()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JDBCTask#isAutocommit()}
   */
  @Test
  public void testIsAutocommit_givenSQLExec_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SQLExec()).isAutocommit());
  }

  /**
   * Test {@link JDBCTask#getUrl()}.
   * <p>
   * Method under test: {@link JDBCTask#getUrl()}
   */
  @Test
  public void testGetUrl() {
    // Arrange, Act and Assert
    assertNull((new SQLExec()).getUrl());
  }

  /**
   * Test {@link JDBCTask#getUserId()}.
   * <p>
   * Method under test: {@link JDBCTask#getUserId()}
   */
  @Test
  public void testGetUserId() {
    // Arrange, Act and Assert
    assertNull((new SQLExec()).getUserId());
  }

  /**
   * Test {@link JDBCTask#setUserid(String)}.
   * <p>
   * Method under test: {@link JDBCTask#setUserid(String)}
   */
  @Test
  public void testSetUserid() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    sqlExec.setUserid("42");

    // Assert
    assertEquals("42", sqlExec.getUserId());
  }

  /**
   * Test {@link JDBCTask#getPassword()}.
   * <p>
   * Method under test: {@link JDBCTask#getPassword()}
   */
  @Test
  public void testGetPassword() {
    // Arrange, Act and Assert
    assertNull((new SQLExec()).getPassword());
  }

  /**
   * Test {@link JDBCTask#getRdbms()}.
   * <p>
   * Method under test: {@link JDBCTask#getRdbms()}
   */
  @Test
  public void testGetRdbms() {
    // Arrange, Act and Assert
    assertNull((new SQLExec()).getRdbms());
  }

  /**
   * Test {@link JDBCTask#getVersion()}.
   * <p>
   * Method under test: {@link JDBCTask#getVersion()}
   */
  @Test
  public void testGetVersion() {
    // Arrange, Act and Assert
    assertNull((new SQLExec()).getVersion());
  }
}
