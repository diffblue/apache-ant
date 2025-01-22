package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.List;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.junit.Test;

public class ContainerMapperDiffblueTest {
  /**
   * Test {@link ContainerMapper#addConfiguredMapper(Mapper)}.
   * <ul>
   *   <li>Given {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#addConfiguredMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredMapper_givenCutDirsMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    Mapper mapper = new Mapper(new Project());
    mapper.addConfigured(new CutDirsMapper());

    // Act
    chainedMapper.addConfiguredMapper(mapper);

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Test {@link ContainerMapper#addConfiguredMapper(Mapper)}.
   * <ul>
   *   <li>When {@link Mapper#Mapper(Project)} with p is {@link Project} (default constructor) addConfigured {@link ChainedMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#addConfiguredMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredMapper_whenMapperWithPIsProjectAddConfiguredChainedMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    Mapper mapper = new Mapper(new Project());
    mapper.addConfigured(new ChainedMapper());

    // Act
    chainedMapper.addConfiguredMapper(mapper);

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Test {@link ContainerMapper#addConfigured(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then {@link ChainedMapper} (default constructor) Mappers first {@link CutDirsMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured_givenCutDirsMapper_thenChainedMapperMappersFirstCutDirsMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    ChainedMapper fileNameMapper = new ChainedMapper();
    CutDirsMapper fileNameMapper2 = new CutDirsMapper();
    fileNameMapper.addConfigured(fileNameMapper2);

    // Act
    chainedMapper.addConfigured(fileNameMapper);

    // Assert that nothing has changed
    List<FileNameMapper> mappers = fileNameMapper.getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof CutDirsMapper);
    assertSame(fileNameMapper2, getResult);
  }

  /**
   * Test {@link ContainerMapper#addConfigured(FileNameMapper)}.
   * <ul>
   *   <li>Then {@link ChainedMapper} (default constructor) Mappers first {@link ChainedMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured_thenChainedMapperMappersFirstChainedMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    ChainedMapper fileNameMapper = new ChainedMapper();
    ChainedMapper fileNameMapper2 = new ChainedMapper();
    fileNameMapper.addConfigured(fileNameMapper2);

    // Act
    chainedMapper.addConfigured(fileNameMapper);

    // Assert that nothing has changed
    List<FileNameMapper> mappers = fileNameMapper.getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof ChainedMapper);
    assertSame(fileNameMapper2, getResult);
  }

  /**
   * Test {@link ContainerMapper#addConfigured(FileNameMapper)}.
   * <ul>
   *   <li>When {@link ChainedMapper} (default constructor).</li>
   *   <li>Then {@link ChainedMapper} (default constructor) Mappers Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured_whenChainedMapper_thenChainedMapperMappersEmpty() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    ChainedMapper fileNameMapper = new ChainedMapper();

    // Act
    chainedMapper.addConfigured(fileNameMapper);

    // Assert that nothing has changed
    assertTrue(fileNameMapper.getMappers().isEmpty());
  }

  /**
   * Test {@link ContainerMapper#addConfigured(FileNameMapper)}.
   * <ul>
   *   <li>When {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then {@link ChainedMapper} (default constructor) Mappers first is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured_whenCutDirsMapper_thenChainedMapperMappersFirstIsCutDirsMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    CutDirsMapper fileNameMapper = new CutDirsMapper();

    // Act
    chainedMapper.addConfigured(fileNameMapper);

    // Assert
    List<FileNameMapper> mappers = chainedMapper.getMappers();
    assertEquals(1, mappers.size());
    assertSame(fileNameMapper, mappers.get(0));
  }

  /**
   * Test {@link ContainerMapper#add(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then {@link ChainedMapper} (default constructor) Mappers first {@link CutDirsMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#add(FileNameMapper)}
   */
  @Test
  public void testAdd_givenCutDirsMapper_thenChainedMapperMappersFirstCutDirsMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    ChainedMapper fileNameMapper = new ChainedMapper();
    CutDirsMapper fileNameMapper2 = new CutDirsMapper();
    fileNameMapper.addConfigured(fileNameMapper2);

    // Act
    chainedMapper.add(fileNameMapper);

    // Assert that nothing has changed
    List<FileNameMapper> mappers = fileNameMapper.getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof CutDirsMapper);
    assertSame(fileNameMapper2, getResult);
  }

  /**
   * Test {@link ContainerMapper#add(FileNameMapper)}.
   * <ul>
   *   <li>Then {@link ChainedMapper} (default constructor) Mappers first {@link ChainedMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#add(FileNameMapper)}
   */
  @Test
  public void testAdd_thenChainedMapperMappersFirstChainedMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    ChainedMapper fileNameMapper = new ChainedMapper();
    ChainedMapper fileNameMapper2 = new ChainedMapper();
    fileNameMapper.addConfigured(fileNameMapper2);

    // Act
    chainedMapper.add(fileNameMapper);

    // Assert that nothing has changed
    List<FileNameMapper> mappers = fileNameMapper.getMappers();
    assertEquals(1, mappers.size());
    FileNameMapper getResult = mappers.get(0);
    assertTrue(getResult instanceof ChainedMapper);
    assertSame(fileNameMapper2, getResult);
  }

  /**
   * Test {@link ContainerMapper#add(FileNameMapper)}.
   * <ul>
   *   <li>When {@link ChainedMapper} (default constructor).</li>
   *   <li>Then {@link ChainedMapper} (default constructor) Mappers Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#add(FileNameMapper)}
   */
  @Test
  public void testAdd_whenChainedMapper_thenChainedMapperMappersEmpty() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    ChainedMapper fileNameMapper = new ChainedMapper();

    // Act
    chainedMapper.add(fileNameMapper);

    // Assert that nothing has changed
    assertTrue(fileNameMapper.getMappers().isEmpty());
  }

  /**
   * Test {@link ContainerMapper#add(FileNameMapper)}.
   * <ul>
   *   <li>When {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then {@link ChainedMapper} (default constructor) Mappers first is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#add(FileNameMapper)}
   */
  @Test
  public void testAdd_whenCutDirsMapper_thenChainedMapperMappersFirstIsCutDirsMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    CutDirsMapper fileNameMapper = new CutDirsMapper();

    // Act
    chainedMapper.add(fileNameMapper);

    // Assert
    List<FileNameMapper> mappers = chainedMapper.getMappers();
    assertEquals(1, mappers.size());
    assertSame(fileNameMapper, mappers.get(0));
  }

  /**
   * Test {@link ContainerMapper#contains(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ChainedMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#contains(FileNameMapper)}
   */
  @Test
  public void testContains_givenChainedMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    // Act and Assert
    assertFalse(chainedMapper.contains(new CutDirsMapper()));
  }

  /**
   * Test {@link ContainerMapper#contains(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ChainedMapper} (default constructor) addConfigured {@link ChainedMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#contains(FileNameMapper)}
   */
  @Test
  public void testContains_givenChainedMapperAddConfiguredChainedMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(new ChainedMapper());

    // Act and Assert
    assertFalse(chainedMapper.contains(new CutDirsMapper()));
  }

  /**
   * Test {@link ContainerMapper#contains(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link ChainedMapper} (default constructor) addConfigured {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainerMapper#contains(FileNameMapper)}
   */
  @Test
  public void testContains_givenChainedMapperAddConfiguredCutDirsMapper() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(new CutDirsMapper());

    // Act and Assert
    assertFalse(chainedMapper.contains(new CutDirsMapper()));
  }

  /**
   * Test {@link ContainerMapper#getMappers()}.
   * <p>
   * Method under test: {@link ContainerMapper#getMappers()}
   */
  @Test
  public void testGetMappers() {
    // Arrange, Act and Assert
    assertTrue((new ChainedMapper()).getMappers().isEmpty());
  }
}
